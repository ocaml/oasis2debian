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

open OASISTypes
open OASISMessage

module MapString = 
  Map.Make(String)

module MapArch = 
  Map.Make(Arch)

type t = (string MapString.t) MapArch.t

let choose_simple ~ctxt conf = 
  OASISExpr.choose
    (fun nm -> 
       try 
         MapString.find nm conf
       with Not_found as e ->
         error ~ctxt "OASIS variable '%s' not defined" nm;
         raise e)

let create ~ctxt pkg = 

  let add_arch mp arch = 
    let conf = 
      (* Convert assoc list into a map *)
      List.fold_left
        (fun mp (nm, vl) -> MapString.add nm vl mp)
        MapString.empty
        arch.Arch.arch_conf
    in

    let conf = 
      (* Evaluate flag values and add them to conf *)
      List.fold_left 
        (fun conf ->
           function 
             | Flag (cs, flag) ->
                 MapString.add
                   cs.cs_name
                   (string_of_bool 
                      (choose_simple ~ctxt conf flag.flag_default))
                   conf
             | Object _ | Library _ | Executable _ | Doc _ | Test _ | SrcRepo _ ->                 
                 conf)
        conf
        pkg.sections
    in
      MapArch.add arch conf mp
  in

    List.fold_left 
      add_arch 
      MapArch.empty 
      (Arch.all ())

type query_arch = 
        [ 
          `All of bool -> bool -> bool 
          (** Combine results *)

        | `Only of Arch.t
        ]


let choose ~ctxt t arch choice = 
  match arch with 
    | `All f ->
        begin
          let acc = 
            MapArch.fold
              (fun _ conf acc ->
                 (choose_simple ~ctxt conf choice) :: acc)
              t
              []
          in
            match acc with 
              | hd :: tl ->
                  List.fold_left f hd tl
              | [] ->
                  invalid_arg "Expr.choose"
        end

    | `Only arch ->
        choose_simple ~ctxt (MapArch.find arch t) choice


