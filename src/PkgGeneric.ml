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

(** Translate the provided package into its generic counterpart
  * See {!Common.t.pkg_generic} for a definition of what section
  * will be kept.
  *)

open OASISTypes

let create ~ctxt expr pkg =

  let eval =
    Expr.choose ~ctxt expr
      (`All (fun x y -> x || y))
  in

  let sections =
    List.fold_left
      (fun acc e ->
         match e with
           | Object (_, bs, _)
           | Library (_, bs, _)
           | Executable (_, bs, _) ->
               if eval bs.bs_build && eval bs.bs_install then
                 e :: acc
               else
                 acc

           | Doc (_, doc) ->
               if eval doc.doc_build && eval doc.doc_install then
                 e :: acc
               else
                 acc

           | Flag _ | Test _ | SrcRepo _ ->
               e :: acc)
      []
      pkg.sections
  in

    {pkg with sections = List.rev sections}
