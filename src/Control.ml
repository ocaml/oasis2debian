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

open OASISTypes
open Common
open ExtString

let executable_extra_depends =
  Conf.create
    ~cli:"--executable-extra-depends"
    "Extra depends to add to executable package."
    (Conf.Value "")

let library_dev_extra_depends =
  Conf.create
    ~cli:"--library-dev-extra-depends"
    "Extra depends to add to library dev package."
    (Conf.Value "")

let library_runtime_extra_depends =
  Conf.create
    ~cli:"--library-runtime-extra-depends"
    "Extra depends to add to library runtime package."
    (Conf.Value "")

let create ~ctxt t =
  let sep = ",\n  " in

  let build_depends =
    String.concat sep
      (List.map
         BuildDepends.to_string
         t.build_depends)
  in

  let src_name = t.deb_name in

  let exec_depends =
    let base_deps =
      match Conf.get ~ctxt executable_extra_depends with
        | "" -> ""
        | str -> str^", "
    in
      if Conf.get ~ctxt Group.group <> None then
        "adduser, "^base_deps
      else
        base_deps
  in

  let lib_dev_depends = Conf.get ~ctxt library_dev_extra_depends in

  let lib_runtime_depends = Conf.get ~ctxt library_runtime_extra_depends in

  let description =
    let lst =
      List.map
        (fun str ->
           match String.strip str with
             | "" -> "."
             | str -> str)
        (String.nsplit t.description "\n")
    in
    let res =
      String.concat "\n " lst
    in
      if res = "" then
        "."
      else
        res
  in

    debian_with_fn "control"
      (fun chn ->
         let output_content x =
           output_content x chn
         in

         let output_intro ?(suggest_doc=true) deb_pkg =
           output_content
             (interpolate "
Package: $deb_pkg.name
Architecture: $deb_pkg.arch");
           begin
             match suggest_doc, t.deb_doc with
               | true, Some {name = nm} ->
                   output_content
                     (interpolate "\
Suggests: $nm")
               | _ ->
                   ()
           end
         in

           (* Intro: the source package *)
           output_content
             (interpolate "\
Source: $src_name
Section: ocaml
Priority: optional
Maintainer: Debian OCaml Maintainers <debian-ocaml-maint@lists.debian.org>
Uploaders:
  $t.uploader
Build-Depends:
  $build_depends
Standards-Version: 3.9.1
Homepage: $t.homepage
Vcs-Git: git://git.debian.org/git/pkg-ocaml-maint/packages/${src_name}.git
Vcs-Browser: \
http://git.debian.org/?p=pkg-ocaml-maint/packages/${src_name}.git");

           begin
             match t.deb_exec with
               | Some deb_pkg ->
                   output_intro deb_pkg;
                   output_content
                     (interpolate "\
Depends: $exec_depends\${misc:Depends}, \${ocaml:Depends}
Description: $t.pkg.synopsis
 $description");
                   if t.deb_dev <> None then
                     output_content " \
 .
 This package contains command-line tools."

               | None ->
                   ()
           end;

           begin
             match t.deb_dev with
               | Some (deb_dev, deb_runtime) ->
                   output_intro deb_dev;
                   output_content
                     (interpolate "\
Depends: $lib_dev_depends\${ocaml:Depends}, \${misc:Depends}
Provides: \${ocaml:Provides}
Recommends: ocaml-findlib
Description: $t.pkg.synopsis
 $description");

                   output_intro deb_runtime;
                   output_content
                      (interpolate "\
Depends: $lib_runtime_depends\${ocaml:Depends}, \
\${misc:Depends}, \${shlibs:Depends}
Provides: \${ocaml:Provides}
Description: $t.pkg.synopsis
 $description
 .
 This package contains the shared runtime libraries.")

               | None ->
                   ()
           end;

           begin
             match t.deb_doc with
               | Some deb_pkg ->
                   output_intro deb_pkg;
                   output_content
                     (interpolate "\
Section: doc
Depends: \${misc:Depends}
Description: $t.pkg.synopsis
 $description
 .
 This package contains the documentation.")

               | None ->
                   ()
           end)
