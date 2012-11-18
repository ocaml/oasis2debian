(******************************************************************************)
(* oasis2debian: create and maintain a debian/ directory using _oasis         *)
(*                                                                            *)
(* Copyright (C) 2010, OCamlCore SARL, http://www.ocamlcore.com               *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

open OASISTypes
open OASISLicense
open OASISMessage
open Common

let copyrights = 
  Conf.create 
    ~cli:"--copyrights"
    "Copyright holds of the package"
    Conf.ShortInput

let todo ~ctxt msg = 
  OASISMessage.warning ~ctxt "%s" msg;
  "TODO"

let license_exception ~ctxt exc = 
  if exc = ocaml_linking_exception then
   " \
 As a special exception to the GNU Library General Public License, you may
 link, statically or dynamically, a \"work that uses the Library\" with a
 publicly distributed version of the Library to produce an executable file
 containing portions of the Library, and distribute that executable file under
 terms of your choice, without any of the additional requirements listed in
 clause 6 of the GNU Library General Public License.  By \"a publicly
 distributed version of the Library\", we mean either the unmodified Library as
 distributed by upstream author, or a modified version of the Library that is
 distributed under the conditions defined in clause 3 of the GNU Library
 General Public License.  This exception does not however invalidate any other
 reasons why the executable file might be covered by the GNU Library General
 Public License."

 else
    " "^(todo ~ctxt 
           (Printf.sprintf 
              "License exception '%s' not defined"
              (string_of_license_exception exc)))

let license_full ~ctxt t = 
  let see_common = 
    Printf.sprintf "See '/usr/share/common-licenses/%s' for full text."
  in
  let min_ver =
    function
      | Version x | VersionOrLater x -> 
          Some (OASISVersion.string_of_version x)
      | NoVersion -> 
          None
  in
  let todo () = 
    todo ~ctxt 
      (Printf.sprintf 
         "License '%s' not defined"
         (OASISLicense.to_string 
            t.pkg.OASISTypes.license))
  in
  let debian_licenses = 
    [
      apache, 
      [`Ver "2.0", "Aparche-2.0"];
      artistic, 
      [`Any, "Artistic"];
      bsd3, 
      [`Any, "BSD"];
      gpl,
      [`Ver "1", "GPL-1";
       `Ver "2", "GPL-2";
       `Ver "3", "GPL-3";
       `None, "GPL"];
      gfdl,
      [`Ver "1.2", "GFDL-1.2";
       `Ver "1.3", "GFDL-1.3";
       `None, "GFDL"];
      lgpl,
      [`Ver "2", "LGPL-2";
       `Ver "2.1", "LGPL-2.1";
       `Ver "3", "LGPL-3";
       `None, "LGPL"]
    ]
  in
    match t.pkg.OASISTypes.license with 
      | DEP5License l ->
          begin
            let process_one l = 
              try
                begin
                  let vers = List.assoc l.license debian_licenses in
                    try 
                      let ver = min_ver l.version in
                      let _, debian_common = 
                        List.find
                          (fun (ver', _) ->
                             match ver, ver' with 
                               | Some v, `Ver v' -> v = v'
                               | None, `None | _, `Any -> true
                               | None, `Ver _ | Some _, `None -> false)
                          vers
                      in
                        see_common debian_common
                    with Not_found ->
                      todo ()
                end
              with Not_found ->
                begin
                  match t.pkg.OASISTypes.license_file with 
                    | Some fn when Sys.file_exists fn ->
                        begin
                          let chn = 
                            open_in fn
                          in
                          let lst =
                            ref []
                          in
                          let () = 
                            try 
                              while true do 
                                lst := input_line chn :: !lst
                              done
                            with End_of_file ->
                              close_in chn
                          in
                            String.concat "\n " (List.rev !lst)
                        end

                    | _ ->
                        todo ()
                end
            in
            let rec process acc =
              function
                | DEP5Unit l ->
                    (process_one l) :: acc
                | DEP5Or lst | DEP5And lst ->
                    List.fold_left process acc lst
            in
              String.concat "\n" (List.rev (process [] l))
          end

      | OtherLicense _ ->
          todo ()


let create ~ctxt t = 
  let copyrights = 
    if Conf.is_set copyrights then 
      Conf.get ~ctxt copyrights 
    else
      begin
        let sep = "\n           "
        in
        match t.pkg.copyrights with 
          | [] -> 
              todo ~ctxt "No copyrights defined"
          | lst -> 
              String.concat sep lst
      end
  in

  let license = 
    OASISLicense.to_string t.pkg.OASISTypes.license
  in

  let license_full = 
    license_full ~ctxt t
  in

  let license_exception =
    match t.pkg.OASISTypes.license with 
      | DEP5License l ->
          let lst = 
            let rec collect_excpt acc =
              function 
                | DEP5Unit {excption = Some e} ->
                    if not (List.mem e acc) then
                      e :: acc
                    else 
                      acc
                | DEP5Unit _ ->
                    acc
                | DEP5Or lst | DEP5And lst ->
                    List.fold_left collect_excpt acc lst
            in
              collect_excpt [] l
          in
          let sep  = 
            "\n\n"
          in
            if lst <> [] then
              sep ^ (String.concat sep (List.map (license_exception ~ctxt) lst))
            else 
              ""

      | _ ->
          ""
  in

  let year =
    (Unix.gmtime (Unix.gettimeofday ())).Unix.tm_year + 1900
  in

    debian_with_fn "copyright"
      (fun chn ->
         let output_content x = 
           output_content x chn 
         in
           output_content 
             (interpolate "\
Format-Specification: http://svn.debian.org/wsvn/dep/web/deps/dep5.mdwn?op=file&rev=135
Name: $t.deb_name
Maintainer: $t.uploader

Files: *
Copyright: $copyrights
License: $license

 $license_full$license_exception

Files: debian/*
Copyright: (C) ${year,%d} $t.uploader
License: GPL-3+
                
 See '/usr/share/common-licenses/GPL-3' for full text.
"))
