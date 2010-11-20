
open FileUtil
open OASISUtils
open OASISMessage
open OASISTypes

type deb_pkg =
    {
      name: string;
      arch: string;
    }

type t =
  {
    build_depends: string list;
    description:   string;
    homepage:      string;
    uploader:      string;
    deb_name:      string;

    (** Pristine OASIS package *)
    pkg:           OASISTypes.package;

    (** OASIS package with section filtered.
        A section is removed if it is not installed
        or built on all arches
      *)
    pkg_generic:   OASISTypes.package; 

    (** Evaluation environment for OASISExpr *)
    expr:          Expr.t;

    (** Standard Debian package *)
    deb_std:       deb_pkg option;

    (** Library + runtime package *)
    deb_dev:       (deb_pkg * deb_pkg) option;

    (** Doc package *)
    deb_doc:       deb_pkg option;
  }

(** [debian_fn fn] Filename [fn] inside 'debian/'
  *)
let debian_fn = 
  FilePath.concat "debian"

(** Test if the filename exist in "debian/"
  *)
let debian_not_exist fn =
  not (test Exists (debian_fn fn))

(** Only execute [f] if the file doesn't exist 
  *)
let debian_with_fn fn f = 
  if debian_not_exist fn then
    begin
      let real_fn = 
        debian_fn fn
      in
      let () = 
        mkdir ~parent:true (FilePath.dirname real_fn)
      in
      let chn = open_out real_fn
      in
        f chn;
        close_out chn
    end

(** Run a command and file if exit code is non-zero
  *)
let assert_command ~ctxt cmd = 
  info ~ctxt "Running command '%s'" cmd;
  match Sys.command cmd with 
    | 0 -> ()
    | n -> 
        failwithf2
          "Command '%s' exited with code %d" 
          cmd n


let assert_command_output ~ctxt cmd = 
  let chn = 
    info ~ctxt "Running command '%s'" cmd;
    Unix.open_process_in cmd
  in
  let rec read_chn () = 
    try 
      let ln = 
        input_line chn 
      in
        ln :: read_chn ()
    with End_of_file ->
      []
  in
  let res =
    read_chn ()
  in
    match Unix.close_process_in chn with 
      | Unix.WEXITED 0 ->
          res
      | _ ->
          failwithf1
            "Command '%s' exited with non-zero exit code"
            cmd

let output_content str chn = 
  output_string chn (str^"\n")


module MapString = Map.Make(String)

let docdir t = 
  let has_doc = 
    List.exists 
      (function Doc _ -> true | _ -> false) 
      t.pkg.sections
  in
    match has_doc, t.deb_doc, t.deb_dev with 
      | true, Some deb_pkg, _ 
      | true, None, Some (deb_pkg, _) ->
          Some ("/usr/share/doc/"^deb_pkg.name)

      | false, _, _
      | true, None, None ->
          None
