
open OASISTypes
open Common
open ExtString

let create t = 
  let sep = 
    ",\n  "
  in

  let build_depends =
    match t.build_depends with 
      | [] -> ""
      | lst ->
          sep^(String.concat sep t.build_depends)
  in

  let src_name = 
    t.pkg.OASISTypes.name
  in

  let description = 
    let lst = 
      List.map 
        (fun str ->
           match String.strip str with
             | "" -> "."
             | str -> str)
        (String.nsplit t.description "\n")
    in
      String.concat "\n " lst
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
  debhelper (>= 7.0.50~),
  dh-ocaml (>= 0.9.0~)$build_depends
Standards-Version: 3.9.1
Homepage: $t.homepage
Vcs-Git: git://git.debian.org/git/pkg-ocaml-maint/packages/${src_name}.git
Vcs-Browser: http://git.debian.org/?p=pkg-ocaml-maint/packages/${src_name}.git");

           begin
             match t.deb_std with 
               | Some deb_pkg ->
                   output_intro deb_pkg;
                   output_content 
                     (interpolate "\
Depends: \${misc:Depends}, \${ocaml:Depends}
Description: $t.pkg.synopsis
 $description");
                   if t.deb_dev <> None then
                     output_content 
" .
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
Depends: \${ocaml:Depends}, \${misc:Depends}
Provides: \${ocaml:Provides}
Recommends: ocaml-findlib
Description: $t.pkg.synopsis
  $description");

                   output_intro deb_runtime;
                   output_content
                      (interpolate "\
Depends: \${ocaml:Depends}, \${misc:Depends}, \${shlibs:Depends}
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
