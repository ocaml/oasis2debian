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

(** Debian architectures 
  *)

type t = 
      {
        arch_name: string;
        arch_conf: (string * string) list;
      }

let compare t1 t2 = 
  String.compare t1.arch_name t2.arch_name


let rall = 
  ref []

let mk nm conf = 
  let res = 
    {
      arch_name = nm;
      arch_conf = conf;
    }
  in
    rall := res :: !rall;
    res

let linux_i386 = 
  mk "i386" []

let linux_amd64 = 
  mk "amd64" []

(*
    "alpha", linux_alpha;
    "armel", linux_armel;
    "hppa", linux_hppa;
    "hurd-i386", hurd_i386;
    "ia64", linux_ia64;
    "kfreebsd-amd64", kfreebsd_amd64;
    "kfreebsd-i386", kfreebsd_i386;
    "mips", linux_mips;
    "mipsel", linux_mipsel;
    "powerpc", linux_powerpc;
    "s390", linux_s390;
    "sparc", linux_sparc;
*)

let all () = 
  !rall


let to_string {arch_name = nm} =
  nm

(** Handle list of architectures 
  *)
module Spec =
struct

  type arch = t
  type t = [ `All | `Only of arch * arch list ]

  let mem t arch = 
    match t with
      | `All -> true (* Match all arches *)
      | `Only (a, lst) ->
          List.exists
            (fun a' -> compare arch a' = 0) 
            (a :: lst)

  let merge t1 t2 = 
    match t1, t2 with 
      | `All, `Only _ 
      | `Only _, `All 
      | `All, `All ->
          `All

      | t, `Only (a, lst) ->
          begin
            List.fold_left
              (fun t e ->
                 match t, mem t e with 
                   | `Only (a, lst), false ->
                       `Only (a, e :: lst)
                   | t, _ ->
                       t)
              t
              (a :: lst)
          end


  let to_string_build_depends = 
    function
      | `All -> 
          ""
      | `Only (hd, tl) as t ->
          begin
            (* All arches of the package *)
            let lst = hd :: tl in 
            let neg =
              List.map 
                (fun arch -> "!"^(to_string arch)) 
                (List.filter 
                   (* Remove arches of the package *)
                   (fun arch -> not (mem t arch)) 
                   (* All arches *)
                   (all ()))
            in
            let lst = 
              List.map to_string lst
            in

            let lst =
              if List.length neg < List.length lst then
                neg
              else
                lst
            in            
              Printf.sprintf " [%s]" (String.concat ", " lst)
          end
end

