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


open OASISMessage

let () = 
  let () = 
    (* Clean ENV *)
    Unix.putenv "OCAMLPATH" "";
    Unix.putenv "LC_ALL" "C"
  in

  let ctxt = 
    {(!OASISContext.default) with 
         OASISContext.ignore_plugins = true}
  in

    if Array.length Sys.argv >= 2 then
      begin
        let args = 
          Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1)
        in
        let run =
          match Sys.argv.(1) with 
            | "init" ->
                ActInit.run
            | "get" ->
                ActGet.run
            | "update" ->
                ActUpdate.run
            | "help" ->
                ActHelp.run
            | str ->
                ActHelp.display ~ctxt stderr;
                error ~ctxt "No action %s defined.  Try \"help\"." str;
                exit 2
        in
          run ~ctxt args
      end
    else
      begin
        ActHelp.display ~ctxt stderr;
        error ~ctxt "Not enough arguments";
        exit 2
      end

