
open FileUtil

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
    pkg:           OASISTypes.package;

    (* Standard Debian package *)
    deb_std:       deb_pkg option;

    (* Library + runtime package *)
    deb_dev:       (deb_pkg * deb_pkg) option;

    (* Doc package *)
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
let assert_command cmd = 
  match Sys.command cmd with 
    | 0 -> ()
    | n -> failwith (Printf.sprintf "Command '%s' exited with code %d" cmd n)

let output_content str chn = 
  output_string chn (str^"\n")

