
open Common

let no_manpage = 
  let lst = ref [] in
    Conf.create_full
      ~cli:"--no-manpage"
      (fun s -> lst := s :: !lst; !lst)
      "exec Disable lintian warning for the given exec without a manpage."
      (Conf.Value !lst)

let add lintian_id arg = 
  debian_with_append_fn
    "lintian-overrides"
    (output_content (Printf.sprintf "%s %s" lintian_id arg))

let create ~ctxt t =
  List.iter (add "binary-without-manpage") (Conf.get ~ctxt no_manpage)
