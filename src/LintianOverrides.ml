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

let no_manpage =
  let lst = ref [] in
    Conf.create_full
      ~cli:"--no-manpage"
      (fun s -> lst := s :: !lst; !lst)
      "exec Disable lintian warning for the given exec without a manpage."
      (Conf.Value !lst)

let add pkg lintian_id arg_opt =
  debian_with_append_fn
    (pkg^".lintian-overrides")
    (output_content
       (match arg_opt with
          | Some arg -> Printf.sprintf "%s %s" lintian_id arg
          | None -> lintian_id))

let create ~ctxt t =
  List.iter
    (fun arg ->
       match t.deb_exec with
         | Some {Common.name = pkg} ->
             add pkg "binary-without-manpage" (Some arg)
         | None ->
             failwith "--no-manpage require a -bin package.")
    (Conf.get ~ctxt no_manpage);
  if not (Conf.is_set Changelog.itp) then
    begin
      (* TODO: don't do that if the package is not new. *)
      let packages =
        List.flatten
          [
            begin
              match t.deb_exec with
                | Some {Common.name = name} -> [name]
                | None -> []
            end;

            begin
              match t.deb_dev with
                | Some ({Common.name = name_dev},
                        {Common.name = name_runtime}) ->
                    [name_dev; name_runtime]
                | None ->
                    []
            end;

            begin
              match t.deb_doc with
                | Some {Common.name = name} -> [name]
                | None -> []
            end;
          ]
      in
        List.iter
          (fun pkg ->
             add pkg "new-package-should-close-itp-bug" None)
          packages
    end
