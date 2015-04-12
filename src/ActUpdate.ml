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

(** Update action
  *)

open OASISMessage
open OASISTypes
open DebianFormats
open Common

module S = BuildDepends.SetDepends

let diff_depends ~ctxt lst1 lst2 =
  let to_set lst =
    List.fold_left (fun st e -> S.add e st) S.empty lst
  in
  (* Compute dependency added/removed *)
  let st1 = to_set lst1 in
  let st2 = to_set lst2 in
  let adds = S.diff st2 st1 in
  let dels = S.diff st1 st2 in

    (* TODO: compute dependency upgraded/downgraded *)
    S.iter
      (fun b ->
         warning ~ctxt
           "New dependency: %s"
           (BuildDepends.to_string b))
      adds;
    S.iter
      (fun b ->
         warning ~ctxt
           "Dependency removed: %s"
           (BuildDepends.to_string b))
      dels

let run ~ctxt args =

  let t =
    Load.load ~ctxt args
  in

  (* TODO: move this to debian-formats *)
  let with_fn fn f =
    let chn =
      open_in fn
    in
      try
        let res =
          f (IO.input_channel chn)
        in
          close_in chn;
          res
      with e ->
        close_in chn;
        raise e
  in

  let ctl_source, ctl_binaries =
    with_fn "debian/control" Control.parse
  in

  let changelog =
    with_fn "debian/changelog" Changelog.head
  in

  let ctl_build_depends =
    List.map
      (function
         | ((pkg, None), _) :: _ ->
             pkg, None, `All
         | ((pkg, Some (op, ver)), _) :: _ ->
             pkg,
             Some (OASISVersion.comparator_of_string (op^" "^ver)),
             `All
         | [] ->
             invalid_arg "ctl_build_depends")
      ctl_source.Control.build_depends
  in

  let () =
    diff_depends ~ctxt ctl_build_depends t.build_depends
  in

  let () =
    let oasis_version =
      OASISVersion.string_of_version t.pkg_generic.version
    in
      (* TODO: take into account EPOCH et al *)
      if oasis_version <> changelog.Changelog.version then
        warning ~ctxt
          "New version '%s', run 'dch -v %s-1 \"New upstream release\"'"
          oasis_version oasis_version
  in

    ()

