
open OASISVersion

(*
let debug_compare cmp = 
  Format.fprintf Format.std_formatter
    "@[<v>Comparator '%s':@ @[%a@]@]\n"
    (string_of_comparator cmp)
    (fun fmt cmp -> ODN.pp_odn fmt cmp)
    (odn_of_comparator cmp)
 *)

let comparator_reduce cmp = 

  let vmin v1 v2 = 
    if version_compare v1 v2 < 0 then
      v1
    else
      v2
  in

  let vmax v1 v2 =
    if version_compare v1 v2 < 0 then
      v2
    else
      v1
  in

  let rec reduce cmp = 
(*
    debug_compare cmp;
 *)
    match cmp with
      (* TODO: this can be improved to reduce more *)
      | VAnd (VGreater v1, VGreater v2) ->
          VGreater (vmax v1 v2)
      | VAnd (VGreaterEqual v1, VGreaterEqual v2) ->
          VGreaterEqual (vmax v1 v2)

      | VAnd (VLesser v1, VLesser v2) ->
          VLesser (vmin v1 v2)
      | VAnd (VLesserEqual v1, VLesserEqual v2) ->
          VLesserEqual (vmin v1 v2)

      | VAnd (c1, c2) ->
          begin
            match reduce c1, reduce c2 with 
              | c1', c2' when c1' = c2' -> c1'
              | c1', c2' -> VAnd (c1', c2')
          end

      | VOr (c1, c2) ->
          begin
            match reduce c1, reduce c2 with
              | c1', c2' when c1' = c2' -> c1'
              | c1', c2' -> VOr (c1', c2')
          end

      | cmp ->
          cmp
  in
    reduce cmp


(*
let red_cmp str = 
  Printf.printf "%s -> %s"
    str
    (string_of_comparator (comparator_reduce (comparator_of_string str)))


let () = 
  List.iter red_cmp 
    [
      ">= 3.10.2 || >= 3.11.1-3~";
    ]
 *)
