
(** Update action 
  *)

open DebianFormats
open Common

let run ~ctxt args = 

  let t = 
    Load.load ~ctxt args
  in

  let ctl_source, ctl_binaries = 
    let chn = 
      open_in "debian/control"
    in
      try 
        let res =
          Control.parse (IO.input_channel chn)
        in
          close_in chn;
          res
      with e ->
        close_in chn;
        raise e
  in
  let ctl_build_depends =
    List.map 
      (function
         | ((pkg, None), _) :: _ -> pkg
         | ((pkg, Some (op, ver)), _) :: _ -> pkg^" ("^op^" "^ver^")"
         | [] -> "<unknown>")
      ctl_source.Control.build_depends
  in

    prerr_endline ("oasis: "^(String.concat ", " t.build_depends));
    prerr_endline ("control: "^(String.concat ", " ctl_build_depends))
