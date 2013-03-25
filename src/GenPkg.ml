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

(** Compute package to generate 
  *)

open OASISTypes
open Common

let library_name = 
  Conf.create 
    ~cli:"--library-name"
    "pkg_name Short name of the library (XXX in libXXX-ocaml-dev)."
    (Conf.Fun
       (fun () ->
          failwith "Not set"))

let executable_name = 
  Conf.create 
    ~cli:"--executable-name"
    "pkg_name Full name of the package that contains executables."
    (Conf.Fun
       (fun () ->
          failwith "Not set"))

let set ~ctxt t = 

  let obj, lib, doc, bin =
    List.fold_left
      (fun ((obj, lib, doc, bin) as acc) ->
         function
           | Object (cs, bs, obj') ->
               ((cs, bs, obj') :: obj), lib, doc, bin

           | Library (cs, bs, lib') ->
               obj, ((cs, bs, lib') :: lib), doc, bin

           | Executable (cs, bs, exec) ->
               obj, lib, doc, ((cs, bs, exec) :: bin)

           | Doc (cs, doc') ->
               obj, lib, ((cs, doc') :: doc), bin

           | Flag _ | Test _ | SrcRepo _ ->
               acc)
      ([], [], [], [])
      t.pkg_generic.sections
  in

  let arch lst = 
    let is_all = 
      List.for_all
        (function
           |  {bs_compiled_object = Byte} ->
               true

           | _ -> 
               false)
        (List.rev_map 
           (fun (_, bs, _) -> bs)
           lst)
    in
      if is_all then
        "all"
      else
        "any"
  in

  let mk_deb nm lst = 
    {name = nm; arch = arch lst}
  in

  let base_name = 
    if Conf.is_set library_name then
      begin
        Conf.get ~ctxt library_name 
      end
    else
      begin
        let groups, _, _ = OASISFindlib.findlib_mapping t.pkg_generic in
        match groups with 
          | [hd] ->
              (* First method: if there is a single findlib library use its name
               *)
              OASISFindlib.findlib_of_group hd

          | _ ->
              (* Default method: try to guess the target name using source name 
               *)
              List.fold_left
                (fun name pat -> 
                   Pcre.replace ~pat ~templ:"" name)

                (* Start with the package name *)
                t.pkg_generic.OASISTypes.name
                ["^ocaml-?"; "-?ocaml$"]
      end
  in

  let spf fmt = 
    Printf.sprintf fmt
  in

  let add_doc nm t = 
    (* Add doc package, only if more than one documentation
     * shipped.
     *)
    if List.length doc > 1 then
      {t with 
           deb_doc = Some (mk_deb nm [])}
    else
      t
  in

  let t =
    (* Determine if we have bin+dev+runtime *)
    match lib, obj, bin with
      | [], [], bin ->
          begin
            (* Only a binary package, name = source name *)
            let exec_name = 
              if Conf.is_set executable_name then 
                Conf.get ~ctxt executable_name 
              else
                t.deb_name
            in
              add_doc 
                (base_name^"-doc")
                {t with deb_exec = Some (mk_deb exec_name bin)} 
          end

      | lib, obj, bin ->
          begin 
            (* Library only *)
            let t  = 
              {t with 
                   deb_dev = 
                     Some (mk_deb (spf "lib%s-ocaml-dev" base_name) lib,
                           mk_deb (spf "lib%s-ocaml" base_name) lib)}
            in

            (* Also executables ? *)
            let t =
              if bin <> [] then
                let exec_name = 
                  if Conf.is_set executable_name then
                    Conf.get ~ctxt executable_name
                  else
                    spf "lib%s-ocaml-bin" base_name
                in
                  {t with deb_exec = Some (mk_deb exec_name bin)}
              else
                t
            in

              add_doc 
                (spf "lib%s-ocaml-doc" base_name)
                t
          end
  in

    t
