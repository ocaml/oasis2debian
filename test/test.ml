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

let oasis2debian =
  ref "_build/src/oasis2debian"

let args_for_tarball =
  ["ocamlify-0.0.1.tar.gz", 
   ["--homepage"; "http://forge.ocamlcore.org/projects/ocamlify"];
  
   "ocamlmod-0.0.3.tar.gz",
   ["--homepage"; "http://forge.ocamlcore.org/projects/ocamlmod"];

   "sekred-0.1.0.tar.gz",
   ["--executable-name"; "sekred";
		"--group"; "sekred,/var/lib/sekred";
		"--dh-dirs"; "sekred,var/lib/sekred/domains";
		"--dpkg-statoverride"; "/usr/bin/sekred,root,sekred,2755";
		"--dpkg-statoverride"; "/var/lib/sekred/domains,root,sekred,1730"];
  ]

let tests = 
  let pwd = pwd () in
  let tarballs = 
    filter 
      (Has_extension "gz")
      (ls (make_filename [pwd; "test"; "data"]))
  in
  let topdir tarball =
    chop_extension (chop_extension (basename tarball))
  in
    List.map 
      (fun tarball ->
         (basename tarball) >::
         (fun ctxt ->
           bracket_tmpdir
             (fun dn ->
                bracket
                  (fun () -> Sys.chdir dn)
                  (fun () ->
                     let () = 
                       assert_command ~ctxt "tar" ["xzf"; tarball];
                     in
                     let pkg =
                       OASISParse.from_file 
                         ~ctxt:{!OASISContext.default with 
                                    OASISContext.ignore_plugins = true}
                         (Filename.concat (topdir tarball) "_oasis")
                     in
                     let pkg_name = pkg.OASISTypes.name in
                     let pkg_ver = OASISVersion.string_of_version pkg.OASISTypes.version in
                     let () = 
                       cp [tarball] 
                         (Filename.concat 
                            dn 
                            (Printf.sprintf "%s_%s.orig.tar.gz" pkg_name pkg_ver));
                       Sys.rename (topdir tarball) pkg_name;
                       Sys.chdir pkg_name;
                     in
                     let args = 
                       try 
                         List.assoc (basename tarball) args_for_tarball
                       with Not_found ->
                         []
                     in
                       assert_command ~ctxt !oasis2debian ("init" :: args);
                       assert_command ~ctxt "debuild" ["-uc"; "-us"];
                       Sys.chdir "..";
                       assert_command ~ctxt "lintian" ("--fail-on-warnings" :: (filter (Has_extension "changes") (ls ".")))
                  )
                  (fun () -> Sys.chdir pwd)
                  ())
             ()))
      tarballs
      

let () =
  Unix.putenv "EDITOR" "true";
  run_test_tt_main
    ~arg_specs:["-exec",
                Arg.Set_string oasis2debian,
                "prg oasis2debian program to test"]
    ("oasis2debian" >::: tests)
