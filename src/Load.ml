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


(** Create a [Common.t] datastructure, using available data
  *)

open OASISMessage
open OASISTypes
open Common
open FileUtil

let description =
  Conf.create
    ~cli:"--description"
    "str Long description of the package"
    Conf.LongInput

let homepage =
  Conf.create
    ~cli:"--homepage"
    "url Homepage of the package"
    Conf.ShortInput

let uploader =
  Conf.create
    ~cli:"--uploader"
    "email Uploader of the package"
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
    "str Source package name in Debian (e.g. extunix becomes ocaml-extunix)"
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

  let dflt r f x =
    match x, Conf.is_set r with
      | Some e, false ->
          Conf.set r (f e)
      | _ ->
          ()
  in

  let () =
    dflt description OASISText.to_string pkg.OASISTypes.description;
    dflt homepage    (fun s -> s) pkg.OASISTypes.homepage
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
    try
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
    with Arg.Help str ->
      print_endline str;
      raise (ExitCode 0)
  in

  let findlib_packages =
    let lst =
      BuildDepends.depends_of_all_arches ~ctxt pkg
        (BuildDepends.SetExec.empty, BuildDepends.SetFindlib.empty)
    in
    let findlib_set =
      List.fold_left
        (fun findlib_set (_, (_, findlib_set')) ->
           BuildDepends.SetFindlib.union findlib_set findlib_set')
        BuildDepends.SetFindlib.empty lst
    in
      List.map fst (BuildDepends.SetFindlib.elements findlib_set)
  in

  let t =
    {
      build_depends   = BuildDepends.get ~ctxt pkg [];
      findlib_packages = findlib_packages;
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
