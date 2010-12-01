(********************************************************************************)
(*  oasis2debian: create and maintain a debian/ directory using _oasis          *)
(*                                                                              *)
(*  Copyright (C) 2010, OCamlCore SARL, http://www.ocamlcore.com                *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

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
    Printf.sprintf "See '/usr/share/common-licenses/%s'."
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
    match t.pkg.OASISTypes.license with 
      | DEP5License l ->
          begin
            let license = 
              l.OASISLicense.license
            in
              if license = apache then
                begin
                  match min_ver l.version with 
                    | Some "2.0" -> see_common "Apache-2.0"
                    | _ -> todo ()
                end

              else if license = artistic then
                begin
                  see_common "Artistic"
                end

              else if license = bsd3 then 
                begin
                  see_common "BSD"
                end

              else if license = gpl then
                begin
                  match min_ver l.version with 
                    | Some "1" -> see_common "GPL-1"
                    | Some "2" -> see_common "GPL-2"
                    | Some "3" -> see_common "GPL-3"
                    | None -> see_common "GPL"
                    | Some _ -> todo ()
                end

              else if license = gfdl then
                begin
                  match min_ver l.version with 
                    | Some "1.2" -> see_common "GFDL-1.2"
                    | Some "1.3" -> see_common "GFDL-1.3"
                    | None -> see_common "GFDL"
                    | Some _ -> todo ()
                end

              else if license = lgpl then
                begin
                  match min_ver l.version with 
                    | Some "2" -> see_common "LGPL-2"
                    | Some "2.1" -> see_common "LGPL-2.1"
                    | Some "3" -> see_common "LGPL-3"
                    | None -> see_common "LGPL"
                    | Some _ -> todo ()
                end

              else
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
      | DEP5License {exceptions = lst} when lst <> [] ->
          let sep  = 
            "\n\n"
          in
            sep ^ (String.concat sep (List.map (license_exception ~ctxt) lst))

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
License: GPL-3+"))
