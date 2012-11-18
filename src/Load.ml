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


(** Create a [Common.t] datastructure, using available data
  *)

open OASISMessage
open OASISTypes
open Common
open FileUtil

let description = 
  Conf.create 
    ~cli:"--description"
    "Long description of the package"
    Conf.LongInput

let homepage =
  Conf.create 
    ~cli:"--homepage"
    "Homepage of the package"
    Conf.ShortInput

let uploader =
  Conf.create 
    ~cli:"--uploader"
    "Uploader of the package"
    (Conf.Fun
       (fun () ->
          try 
            Printf.sprintf 
              "%s <%s>"
              (Sys.getenv "DEBFULLNAME")
              (Sys.getenv "DEBEMAIL")
          with _ ->
            failwith "Unable to guess the identity of the package maintainer. \
                      Please set the environment variables DEBFULLNAME and \
                      DEBEMAIL or use --uploader"))

let deb_name =
  Conf.create 
    ~cli:"--debian-name"
    "Source package name in Debian (e.g. extunix becomes ocaml-extunix)"
    Conf.ShortInput


let load ~ctxt args = 

  let pkg = 
    OASISParse.from_file
      ~ctxt
      "_oasis"
  in
                 
  let expr = 
    Expr.create ~ctxt pkg
  in

  let pkg_generic =
    PkgGeneric.create ~ctxt expr pkg
  in

  let dflt r x =
    match x, Conf.is_set r with 
      | Some e, false -> 
          Conf.set r e
      | _ ->
          ()
  in

  let () = 
    dflt description pkg.OASISTypes.description;
    dflt homepage    pkg.OASISTypes.homepage
  in

  let () = 
    let cur_dn = FilePath.basename (pwd ()) in
    let pkg_nm = pkg.OASISTypes.name in
      if pkg_nm = cur_dn then
        Conf.set deb_name cur_dn
      else
        warning ~ctxt 
          "OASIS name (%s) and directory name (%s) are not the same, \
           cannot set Debian name"
          pkg_nm cur_dn
  in

  let () = 
    Arg.parse_argv
      ~current:(ref 0)
      args
      (Arg.align !Conf.all_args)
      (fun s -> 
         failwith 
           (Printf.sprintf
              "Don't know what to do with '%s'"
              s))
      (Printf.sprintf 
         "oasis2debian v%s by Sylvain Le Gall"
         Version.ver)
  in

  let t = 
    {
      build_depends = BuildDepends.get ~ctxt pkg [];
      description   = Conf.get ~ctxt description;
      homepage      = Conf.get ~ctxt homepage;
      uploader      = Conf.get ~ctxt uploader;
      deb_name      = Conf.get ~ctxt deb_name;
      pkg           = pkg;
      pkg_generic   = pkg_generic;
      expr          = expr;
      deb_exec      = None;
      deb_dev       = None;
      deb_doc       = None;
    }
  in

  let t = 
    (* Fix package generated *)
    GenPkg.set ~ctxt t
  in

    t
