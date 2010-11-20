
open Common

(* Create debian/rules *)
let create t = 
  let destdir = 
    match t.deb_std, t.deb_dev, t.deb_doc with
      | Some deb_pkg, None, None 
      | _, _, Some deb_pkg ->
          (* Only one package, move data directly into 
           * it
           *)
          deb_pkg.name
      | _, _, _ ->
          (* More than 1 package, we need to install files
           * in different packages 
           *)
          "tmp"
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
         (interpolate "\
#!/usr/bin/make -f
# -*- makefile -*-

# Uncomment this to turn on verbose mode.
#export DH_VERBOSE=1

DESTDIR=\$(CURDIR)/debian/$destdir

include /usr/share/ocaml/ocamlvars.mk

OCAMLFIND_DESTDIR=\$(DESTDIR)/\$(OCAML_STDLIB_DIR)
export OCAMLFIND_DESTDIR
OCAMLFIND_LDCONF=ignore
export OCAMLFIND_LDCONF

%:
	dh --with ocaml \$@

.PHONY: override_dh_auto_configure
override_dh_auto_configure:
	ocaml setup.ml -configure --prefix /usr --destdir '\$(DESTDIR)'$docdir

.PHONY: override_dh_auto_build
override_dh_auto_build:
	ocaml setup.ml -build
	ocaml setup.ml -doc

.PHONY: override_dh_auto_test
override_dh_auto_test:
	ocaml setup.ml -test

.PHONY: override_dh_auto_install
override_dh_auto_install:
	mkdir -p '\$(DESTDIR)/usr/bin'
	mkdir -p '\$(OCAMLFIND_DESTDIR)'
	ocaml setup.ml -install 

.PHONY: override_dh_install
override_dh_install:
	dh_install --fail-missing

.PHONY: override_dh_auto_clean
override_dh_auto_clean:
	ocaml setup.ml -distclean"));
    Unix.chmod (debian_fn "rules") 0o0755 
