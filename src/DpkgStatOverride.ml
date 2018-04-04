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

type t =
    {
      fn: string;
      owner: string;
      group: string;
      acl: string;
    }

let dpkg_statoverride =
  let lst = ref [] in
    Conf.create_full
      ~cli:"--dpkg-statoverride"
      (fun s ->
         Scanf.sscanf
           s "%s@,%s@,%s@,%s"
           (fun fn owner group acl ->
              let lst' =
                {fn = fn; owner = owner; group = group; acl = acl}
                :: !lst
              in
                lst := lst';
                lst'))
      "fn,owner,group,acl Create a dpkg-statoverride entry (postinst/postrm)."
      (Conf.Value !lst)

let create ~ctxt t =
  match Conf.get ~ctxt dpkg_statoverride, t.deb_exec with
    | _ :: _, None ->
        failwith "--dpkg-statoverride called without an executable package."
    | [], None ->
        ()
    | lst, Some {name = exec_name; _} ->
        List.iter
          (fun t ->
             let snippet_name = Printf.sprintf "dpkg-statoverride(%s)" t.fn in
               DhFiles.dh_postinst
                 exec_name
                 snippet_name
                 ("\
if [ \"$1\" = configure ]; then
  if ! dpkg-statoverride --list '"^t.fn^"' >/dev/null 2>&1; then
    chown '"^t.owner^":"^t.group^"' '"^t.fn^"'
    chmod "^t.acl^" '"^t.fn^"'
  fi
fi");
               DhFiles.dh_prerm
                 exec_name
                 snippet_name
                 ("\
if [ \"$1\" = remove ]; then
  rm -rf '"^t.fn^"' || true
fi"))
          (List.rev lst)

