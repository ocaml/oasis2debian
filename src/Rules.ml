
open Common

(* Create debian/rules *)
let create t = 
  let destdir = 
    match t.deb_std, t.deb_dev, t.deb_doc with
      | Some _, None, None 
      | _, _, Some _ ->
          (* Only one package, move data directly into 
           * it
           *)
          t.pkg.OASISTypes.name
      | _, _, _ ->
          (* More than 1 package, we need to install files
           * in different packages 
           *)
          "temp"
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

%:
	dh --with ocaml \$@

.PHONY: override_dh_auto_configure
override_dh_auto_configure:
	ocaml setup.ml -configure --prefix /usr --destdir '\$(DESTDIR)' --enable-debug

.PHONY: override_dh_auto_build
override_dh_auto_build:
	ocaml setup.ml -build

.PHONY: override_dh_auto_install
override_dh_auto_install:
	mkdir -p '\$(DESTDIR)/usr/bin'
	ocaml setup.ml -install 

.PHONY: override_dh_auto_clean
override_dh_auto_clean:
	ocaml setup.ml -distclean"));
    Unix.chmod (debian_fn "rules") 0o655 
