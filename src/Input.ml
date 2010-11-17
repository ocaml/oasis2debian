
open ExtString
open Common
open FileUtil

(** Edit a text in an editor *)
let long text help = 
  let fn, chn = 
    Filename.open_temp_file "oasis2debian-" ".txt"
  in

  let read_content () = 
    let chn = 
      open_in fn 
    in 
    let str =
      String.make (in_channel_length chn) 'X'
    in
    let () =
      really_input chn str 0 (String.length str);
      close_in chn
    in
    let lst = 
      List.filter
        (fun s -> not (String.starts_with s "#"))
        (String.nsplit str "\n")
    in

    let rec chop_blank_eol str = 
      if String.ends_with str " " || 
         String.ends_with str "\t" ||
         String.ends_with str "\r" then
        chop_blank_eol (String.rchop str) 
      else
        str
    in

    let lst = 
      List.rev_map chop_blank_eol lst
    in
      
    let rec chop_blank_last_lines = 
      function
        | "" :: tl -> 
            chop_blank_last_lines tl
        | lst ->
            List.rev lst
    in

    let lst =
      chop_blank_last_lines lst
    in

      String.concat "\n" lst
  in


    try 
      begin
        let res = 
          (* Write the initial content *)
          output_string chn text;
          output_string chn
            (String.concat "\n# "
               ("" :: 
                "Type your text above, lines starting with '#' will be ignored" ::
                (String.nsplit help "\n")));
          close_out chn;

          (* Run the editor (debian specific) *)
          assert_command (Printf.sprintf "sensible-editor %s" fn);

          (* Get back the text *)
          read_content ()
        in
          rm [fn];
          res
      end

    with e ->
      rm [fn];
      raise e

let short q =
  print_string q; flush stdout;
  read_line ()
