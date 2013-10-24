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

open OUnit2
open FileUtil
open FilePath

let oasis2debian = Conf.make_exec "oasis2debian"
let tar = Conf.make_exec "tar"
let debuild = Conf.make_exec "debuild"
let lintian = Conf.make_exec "lintian"

let args_for_tarball =
  ["ocamlify-0.0.1.tar.gz",
   ["--homepage"; "http://forge.ocamlcore.org/projects/ocamlify";
    "--no-manpage"; "usr/bin/ocamlify";
    "--description"; "Extended description of ocamlify."];

   "ocamlmod-0.0.3.tar.gz",
   ["--homepage"; "http://forge.ocamlcore.org/projects/ocamlmod"];

   "sekred-0.1.0.tar.gz",
   ["--executable-name"; "sekred";
    "--group"; "sekred,/var/lib/sekred";
    "--dh-dirs"; "sekred,var/lib/sekred/domains";
    "--dpkg-statoverride"; "/usr/bin/sekred,root,sekred,2755";
    "--dpkg-statoverride"; "/var/lib/sekred/domains,root,sekred,1730";
    "--no-manpage"; "usr/bin/sekred"];
  ]

let check_file_style test_ctxt fn =
  let chn = open_in fn in
    logf test_ctxt `Info "File: %s" (Filename.basename fn);
    try
      while true do
        let line = input_line chn in
        let strlen = String.length line in
          logf test_ctxt `Info "%s" line;
          if strlen > 0 then
            assert_bool
              (Printf.sprintf "No blank at the end of line %S." line)
              (line.[strlen - 1] <> ' ')
      done
    with End_of_file ->
      close_in chn

let check_debian_dir_style test_ctxt dn =
  find Is_file dn (fun () -> check_file_style test_ctxt) ()

let tests =
  let pwd = pwd () in
  let tarballs =
    filter
      (Has_extension "gz")
      (ls (make_filename [pwd; "test"; "data"]))
  in
    List.map
      (fun tarball ->
         (basename tarball) >::
         (fun test_ctxt ->
            let dn = bracket_tmpdir test_ctxt in
            let topdir =
              Filename.concat dn
                (chop_extension (chop_extension (basename tarball)))
            in
            let () =
              assert_command ~chdir:dn ~ctxt:test_ctxt
                (tar test_ctxt) ["xzf"; tarball];
            in
            let pkg =
              OASISParse.from_file
                ~ctxt:{!OASISContext.default with
                           OASISContext.ignore_plugins = true}
                (Filename.concat topdir "_oasis")
            in
            let pkg_name = pkg.OASISTypes.name in
            let pkg_ver =
              OASISVersion.string_of_version pkg.OASISTypes.version
            in
            let pkg_dir = Filename.concat dn pkg_name in
            let () =
              cp [tarball]
                (Filename.concat dn
                   (Printf.sprintf "%s_%s.orig.tar.gz" pkg_name pkg_ver));
              Sys.rename topdir pkg_dir;
            in
            let args =
              try
                List.assoc (basename tarball) args_for_tarball
              with Not_found ->
                []
            in
              assert_command ~ctxt:test_ctxt ~chdir:pkg_dir
                (oasis2debian test_ctxt) ("init" :: args);
              check_debian_dir_style test_ctxt
                (Filename.concat pkg_dir "debian");
              assert_command ~ctxt:test_ctxt ~chdir:pkg_dir
                (debuild test_ctxt) ["-uc"; "-us"];
              assert_command ~ctxt:test_ctxt ~chdir:dn
                (lintian test_ctxt)
                ("--fail-on-warnings" ::
                 (filter (Has_extension "changes") (ls ".")))))
      tarballs

let () =
  Unix.putenv "EDITOR" "true";
  run_test_tt_main ("oasis2debian" >::: tests)
