
type 'a t = 
  | Value of 'a
  | LongInput 
  | ShortInput
  | Fun of (unit -> 'a)

type 'a rcrd =
    {
      mutable value: 'a t;
      parse: string -> 'a;
      help: string;
    }

let all_args : ((string * Arg.spec * string) list) ref = 
  ref []

let set r x = 
  r.value <- Value x

let is_set r =
  match r.value with 
    | Value _ -> true
    | _ -> false

let rec get r = 
  match r.value with 
    | Fun f ->
        set r (f ()); 
        get r

    | LongInput ->
        set r (r.parse (Input.long "" r.help));
        get r

    | ShortInput -> 
        set r (r.parse (Input.short (r.help^": ")));
        get r

    | Value x ->
        x

let create ?cli ?parse help t = 
  let parse =
    match parse with 
      | Some f -> 
          f 
      | None -> 
          fun s -> s
  in
  let res = 
    {
      value = t;
      parse = parse;
      help  = help;
    }
  in
    begin
      match cli with 
        | Some cli -> 
            all_args := 
            (cli,
             Arg.String (fun s -> set res (parse s)),
             help)
            :: !all_args
        | None -> ()
    end;
    res
