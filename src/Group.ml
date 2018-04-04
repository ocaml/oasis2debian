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

let group =
  Conf.create_full
    ~cli:"--group"
    (fun s ->
       Scanf.sscanf
         s "%s@,%s"
         (fun group homedir ->
            Some (group, homedir)))
    "group,homedir Create a group for the executable package (postinst/prerm)."
    (Conf.Value None)

let create ~ctxt t =
  match Conf.get ~ctxt group, t.deb_exec with
  | Some (group, homedir), Some {name = exec_name; _} ->
        let snippet_name = Printf.sprintf "group(%s)" group in
          DhDirs.dh_dirs
            exec_name
            homedir;
          DhFiles.dh_postinst
            exec_name
            snippet_name
            ("\
if [ \"$1\" = configure ]; then
  if ! getent group '"^group^"' > /dev/null; then
    adduser --system --quiet --home '"^homedir^"' --no-create-home \\
      --disabled-password --group '"^group^"'
  fi
fi");
          DhFiles.dh_prerm
            exec_name
            snippet_name
            ("\
if [ \"$1\" = remove ]; then
  delgroup '"^group^"' > /dev/null 2>&1 || true
fi")

    | Some _, None ->
        failwith "--group without an executable package."

    | None, _ ->
        ()
