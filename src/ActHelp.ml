open Printf

(** Display help *)
let run ~ctxt args =
  eprintf "%s <command> where command is one of\n"
          (Filename.basename Sys.argv.(0));
  eprintf "  init\n";
  eprintf "  get\n";
  eprintf "  update  update the Debian files for a new version of _oasis.\n";
  eprintf "  help    display this help.\n"

let display ~ctxt _ =
  prerr_endline (Filename.basename Sys.argv.(0))
