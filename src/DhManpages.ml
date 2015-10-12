(******************************************************************************)
(* oasis2debian: Create and maintain Debian package for an OASIS package      *)
(*                                                                            *)
(* Copyright (C) 2013, Sylvain Le Gall                                        *)
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

open Common
open ExtString
open OASISTypes

let dh_manpages pkg dir =
  debian_with_append_fn (pkg^".manpages") (output_content dir)


let doc_is_manpage cs doc =
  String.starts_with doc.doc_install_dir "$mandir/"


let manpage_section cs doc =
  match Filename.basename doc.doc_install_dir with
    | "man1" -> 1 | "man2" -> 2 | "man3" -> 3 | "man4" -> 4
    | "man5" -> 5 | "man6" -> 6 | "man7" -> 7 | "man8" -> 8
    | "man9" -> 9
    | fn ->
        failwith
          (Printf.sprintf
             "Unknown man section for document %S: %S"
             cs.cs_name doc.doc_install_dir)


let manpages t =
  let deb_exec, deb_dev =
    match t.deb_exec, t.deb_dev, t.deb_doc with        
      | Some deb_exec, _, Some deb_dev 
      | Some deb_exec, Some (deb_dev, _), None ->
          deb_exec, deb_dev
      | Some deb, _, None
      | None, _, Some deb 
      | None, Some (deb, _), None ->
          deb, deb
      | None, None, None ->
          assert false
  in
    List.fold_left
      (fun lst ->
         function
           | Doc (cs, doc) ->
               if doc_is_manpage cs doc then begin
                 let fn =
                   FilePath.make_filename
                     [var_expand t doc.doc_install_dir; cs.cs_name]
                 in
                 let deb =
                   match manpage_section cs doc with
                     | 3 | 4 -> deb_dev
                     | _ -> deb_exec
                 in
                  (fn, deb) :: lst
               end else begin
                 lst
               end
           | Object _ | Library _ | Executable _
           | Flag _ | Test _ | SrcRepo _ ->
               lst)
      [] t.pkg_generic.sections


let dh_install_excludes t =
  List.map fst (manpages t)


let create ~ctxt t =
  let in_destdir = in_destdir t in
    List.iter
      (fun (fn, deb) ->
         dh_manpages deb.Common.name (in_destdir fn))
      (manpages t)

