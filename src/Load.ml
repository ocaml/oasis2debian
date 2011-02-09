

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
            failwith "Unable to guess uploader"))

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
