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

(** Init action
  *)

open OASISTypes
open OASISMessage
open FileUtil
open Common

let dh_compat = "10"

let run ~ctxt args =

  let t =
    Load.load ~ctxt args
  in

  (* Create debian/ and debian/compat *)
  let () =
    debian_with_fn "compat"
      (fun chn -> output_string chn (dh_compat^"\n"))
  in

  (* Create debian/gbp.conf *)
  let () =
    debian_with_fn "gbp.conf"
      (fun chn ->
         output_string chn
           "[DEFAULT]\n\
            pristine-tar = True\n\
            cleaner = debuild clean && dh_quilt_unpatch && dh_clean\n")
  in

  (* Create debian/source *)
  let () =
    debian_with_fn "source/format"
      (output_content "3.0 (quilt)")
  in

  let () =
    Changelog.create ~ctxt t;
    Control.create ~ctxt t;
    Copyright.create ~ctxt t;
    Rules.create t;
    DhFiles.create ~ctxt t;
    Group.create ~ctxt t;
    DpkgStatOverride.create ~ctxt t;
    Upgrade.create ~ctxt t;
    DhDirs.create ~ctxt t;
    LintianOverrides.create ~ctxt t;
    DhManpages.create ~ctxt t;
  in
    ()
