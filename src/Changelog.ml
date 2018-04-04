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
open OASISTypes

let itp =
  Conf.create
    ~cli:"--itp"
    "int Bug number of the ITP for the package."
    Conf.ShortInput

let distribution =
  Conf.create
    ~cli:"--distribution"
    "str Distribution for the package."
    (Conf.Value "UNRELEASED")

let create ~ctxt t =
  let pkg_version =
    OASISVersion.string_of_version t.pkg.version
  in
  let date =
    CalendarLib.Printer.Calendar.sprint "%a, %d %b %Y %T %z"
      (CalendarLib.Calendar.now ())
  in
  let distribution = Conf.get ~ctxt distribution in
  let closes =
    if Conf.is_set itp then
      Printf.sprintf " (Closes: #%s)" (Conf.get ~ctxt itp)
    else
      ""
  in
    debian_with_fn
      "changelog"
      (output_content
         ("\
"^t.pkg.name^" ("^pkg_version^"-1) "^distribution^"; urgency=low

  * Initial release."^closes^"
  * Generated with oasis2debian v"^Version.ver^".

 -- "^t.uploader^"  "^date))


