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

open Common

(* Create debian/rules *)
let create t =
  let destdir = destdir t in
  let has_bin =
    t.deb_exec <> None
  in
  let has_lib =
    t.deb_dev <> None
  in

  let docdir =
    match docdir t with
      | Some fn ->
          " --docdir '"^fn^"'"
      | None ->
          ""
  in
    debian_with_fn "rules"
      (output_content
         (("\
#!/usr/bin/make -f
# -*- makefile -*-

# Uncomment this to turn on verbose mode.
#export DH_VERBOSE=1

DESTDIR=$(CURDIR)/"^destdir^"

include /usr/share/ocaml/ocamlvars.mk

OCAMLFIND_DESTDIR=$(DESTDIR)/$(OCAML_STDLIB_DIR)
export OCAMLFIND_DESTDIR
OCAMLFIND_LDCONF=ignore
export OCAMLFIND_LDCONF

%:
	dh --with ocaml $@

.PHONY: override_dh_auto_configure
override_dh_auto_configure:
	ocaml setup.ml -configure "^docdir^" \\
		--destdir '$(DESTDIR)' \\
		--prefix '/usr' \\
		--mandir '$$prefix/share/man' \\
		--infodir '$$prefix/share/info' \\
		--sysconfdir '/etc' \\
		--localstatedir '/var' \\
		--libexecdir '$$prefix/lib/'

.PHONY: override_dh_auto_build
override_dh_auto_build:
	ocaml setup.ml -build
	ocaml setup.ml -doc

.PHONY: override_dh_auto_test
override_dh_auto_test:
	ocaml setup.ml -test

.PHONY: override_dh_auto_install
override_dh_auto_install:")^(
  if has_bin then
    "
	mkdir -p '$(DESTDIR)/usr/bin'"
  else
    "")^(
  if has_lib then
    "
	mkdir -p '$(OCAMLFIND_DESTDIR)'"
  else
    "")^("
	ocaml setup.ml -install

.PHONY: override_dh_install
override_dh_install:
	dh_install --fail-missing")^(
    match  DhManpages.dh_install_excludes t with
      | [] -> ""
      | lst ->
          String.concat " \\\n"
            ("" :: List.map (fun fn -> "-X "^(Filename.quote fn)) lst)
  )^("

.PHONY: override_dh_auto_clean
override_dh_auto_clean:
	ocaml setup.ml -distclean")));
    Unix.chmod (debian_fn "rules") 0o0755
