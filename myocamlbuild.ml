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

(* OASIS_START *)
(* OASIS_STOP *)

(*
let camlmix = 
  (* TODO: we should really auto load the env + Hashtbl *)
  let env = 
    BaseEnvLight.load ()
  in
    try 
      BaseEnvLight.var_get "camlmix" env
    with Not_found ->
      failwith (Printf.sprintf "camlmix not defined")
;;

rule "camlmix: mlx -> ml"
  ~prod:"%.ml"
  ~dep:"%.mlx"
  (fun env _build ->
     Cmd(S[A camlmix; 
           A"-c"; 
           A"-co"; P(env "%.ml"); 
           A"-fun"; 
           P(env "%.mlx")]))
;;
 *)

Ocamlbuild_plugin.dispatch dispatch_default;;
