(********************************************************************************)
(*  oasis2debian: create and maintain a debian/ directory using _oasis          *)
(*                                                                              *)
(*  Copyright (C) 2010, OCamlCore SARL, http://www.ocamlcore.com                *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

open OUnit
open FileUtil
open FilePath

let oasis2debian =
  ref "_build/src/oasis2debian"

let args_for_dir =
  ["ocamlify", 
   ["--homepage"; "http://forge.ocamlcore.org/projects/ocamlify"];

   "ocaml-data-notation", 
   ["--homepage"; "http://forge.ocamlcore.org/projects/odn"]]

let tests = 
  let dirs = 
    ls (make_filename ["test"; "data"])
  in
  let pwd =
    pwd ()
  in
    List.map 
      (fun dn ->
         dn >::
         bracket 
           (fun () ->
              Sys.chdir dn)
           (fun () ->
              let args = 
                try 
                  List.assoc (basename dn) args_for_dir
                with Not_found ->
                  []
              in
                assert_command !oasis2debian args)
           (fun () ->
              rm ~recurse:true ["debian"];
              Sys.chdir pwd))
      dirs
      

let _ =
  Unix.putenv "EDITOR" "true";
  run_test_tt_main
    ~arg_specs:["-exec",
                Arg.Set_string oasis2debian,
                "prg oasis2debian program to test"]
    ("oasis2debian" >::: tests)
