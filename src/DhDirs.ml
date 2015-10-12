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


let dh_dirs pkg dir =
  debian_with_append_fn
    (pkg^".dirs")
    (output_content dir)

let dirs =
  let lst = ref [] in
    Conf.create_full
      ~cli:"--dh-dirs"
      (fun s ->
         Scanf.sscanf
           s "%s@,%s"
           (fun pkg fn ->
              let lst' =
                (pkg, fn) :: !lst
              in
                lst := lst';
                lst'))
      "pkg,fn Add an entry in pkg.dirs."
      (Conf.Value !lst)

let create ~ctxt t =
  List.iter
    (fun (pkg, fn) -> dh_dirs pkg fn)
    (Conf.get ~ctxt dirs)
