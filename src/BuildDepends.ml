
open OASISTypes
open OASISUtils
open OASISMessage
open Common
open ExtString
open FileUtil
open FilePath

module SetExec = 
  Set.Make
    (String)

module SetFindlib = 
  Set.Make
    (struct
       type t = findlib_full * OASISVersion.comparator option
       let compare = Pervasives.compare
     end)

module SetDepends =
  Set.Make
    (struct
       type t = string * OASISVersion.comparator option * Arch.Spec.t
       let compare (nm1, _, _) (nm2, _, _) = 
         String.compare nm1 nm2
     end)

let cmp_opt_merge v1_opt v2_opt = 
  match v1_opt, v2_opt with
    | Some v1, Some v2 -> 
        Some 
          (OASISVersion.comparator_reduce 
             (OASISVersion.VAnd (v1, v2)))
    | None, opt | opt, None ->
        opt

let add_depends ?(arch_spec=`All) nm ver_opt st =
  let ver_opt, arch_spec =
    try 
      let _, ver_opt', arch_spec' = 
        List.find
          (fun (nm', _, _) -> nm' = nm)
          (SetDepends.elements st)
      in
      let ver_opt = 
        cmp_opt_merge ver_opt' ver_opt 
      in
        ver_opt, Arch.Spec.merge arch_spec arch_spec'

    with Not_found ->
      ver_opt, arch_spec
  in

    SetDepends.add (nm, ver_opt, arch_spec) st

let string_of_depends (nm, ver_opt, arch_spec) = 
  let arch_str = 
    Arch.Spec.to_string_build_depends arch_spec
  in
  let ver_str =
    match ver_opt with 
      | Some v ->
          Printf.sprintf " (%s)"
            (OASISVersion.string_of_comparator v)
      | None ->
          ""
  in
    nm^ver_str^arch_str

(* Compute build dependencies, against real debian packages
 *)
let get ~ctxt pkg = 
  let eval = 
    let t = 
      Expr.create ~ctxt pkg
    in
      Expr.choose ~ctxt t 
  in

  let depends_of_arch arch acc = 
    let depends_of_bs bs ((exec, fndlb) as acc) =
      let eval  =
        eval (`Only arch)
      in
        if eval bs.bs_build && eval bs.bs_install then
          begin
            let exec = 
              List.fold_left 
                (fun exec ->
                   function
                     | ExternalTool nm -> 
                         SetExec.add nm exec
                     | InternalExecutable _ ->
                         exec)
                exec
                bs.bs_build_tools
            in

            let fndlb =
              List.fold_left
                (fun fndlb ->
                   function
                     | FindlibPackage (nm, ver_opt) ->
                         SetFindlib.add (nm, ver_opt) fndlb
                     | InternalLibrary _ ->
                         fndlb)
                fndlb
                bs.bs_build_depends
            in

              exec, fndlb
          end
        else
          begin
            acc
          end
    in

      List.fold_left 
        (fun ((exec, fndlb) as acc) ->
           function
             | Library (_, bs, _) 
             | Executable (_, bs, _) ->
                 depends_of_bs bs acc
             
             | Flag _ | Test _ | SrcRepo _ | Doc _ ->
                 acc)
        acc
        pkg.sections
  in

  let ocaml_stdlib_dir = 
    match assert_command_output ~ctxt "ocamlc -where" with
      | hd :: _ ->
          hd
      | [] ->
          failwith
            "Cannot determine ocaml standard library directory"
  in

  let debian_depends_of_depends ?arch_spec exec fndlb st = 
    (* Find file *)
    let find_file ?(dev_pkg=false) fn = 
      let filter lst = 
        if dev_pkg then
          begin
            let lst' = 
              List.filter
                (fun nm -> String.ends_with nm "-dev")
                lst
            in
              (* Some package don't adhere to the -dev convention (e.g. camlp4),
               * so better return something than nothing
               *)
              if lst' <> [] then
                lst'
              else 
                lst
          end
        else
          lst
      in

      let output = 
        try 
          assert_command_output ~ctxt
            (Printf.sprintf "dpkg -S '%s'" fn)
        with _ ->        
          assert_command_output ~ctxt 
            (Printf.sprintf "apt-file search -F '%s'" fn)
      in

      let pkg = 
        List.flatten 
          (List.rev_map 
             (fun line ->
                match String.nsplit line ":" with 
                  | hd :: _ -> 
                      List.rev_map 
                        String.strip 
                        (String.nsplit hd ",")
                  | _ ->
                      [])
             output)
      in

        match filter pkg with 
          | hd :: tl ->
              if tl <> [] then
                warning ~ctxt 
                  "Choose package '%s' but other packages possible: %s"
                  hd 
                  (String.concat ", " 
                     (List.map 
                        (Printf.sprintf "'%s'") 
                        tl));
              hd

          | [] ->
              failwithf1
                "Cannot find file '%s'"
                fn
    in

    (* Find an executable *)
    let find_exec nm = 
      let fn = 
        try 
          which 
            ~path:["/usr/bin"; "/bin"; "/usr/sbin"; "/sbin"]
            nm 
        with Not_found ->
          (* Make a guess *)
          make_filename ["/usr/bin"; nm]
      in
        find_file fn
    in

    (* Find a findlib library *)
    let find_findlib nm =
      let fn = 
        try 
          let parse_output =
            function
              | [] | [""] ->
                  raise Not_found
              | hd :: _ -> 
                  hd
          in

          let archive pred = 
            let output = 
              assert_command_output ~ctxt
                (Printf.sprintf 
                   "ocamlfind query -predicates '%s' %s -format '%%d/%%a'"
                   pred nm)
            in
              parse_output output
          in

            try 
              begin
                archive "byte" 
              end

            with Not_found ->
              begin
                try
                  archive "syntax,toploop"
                with Not_found ->
                  begin
                    let output = 
                      assert_command_output ~ctxt 
                        (Printf.sprintf
                           "ocamlfind query %s -format '%%d/META'"
                           nm)
                    in
                    let fn =
                      parse_output output 
                    in
                      if Sys.file_exists fn then
                        fn
                      else
                        begin
                          warning ~ctxt 
                            "Cannot find installed file for findlib package '%s'" 
                            nm;
                          raise Not_found 
                        end
                  end
              end

        with _ ->
          (* Make a guess *)
          make_filename [ocaml_stdlib_dir; nm]
      in
        find_file ~dev_pkg:true fn
    in

    let st = 
      SetExec.fold
        (fun nm st ->
           add_depends ?arch_spec (find_exec nm) None st)
        exec
        st
    in

      SetFindlib.fold
        (fun (nm, ver_opt) st ->
           add_depends ?arch_spec (find_findlib nm) ver_opt st)
        fndlb
        st
  in

  let lst = 
    List.rev_map
      (fun arch ->
         let arch_spec = 
           `Only (arch, [])
         in
           arch_spec, 
           depends_of_arch
             arch
             (SetExec.empty, SetFindlib.empty))
      (Arch.all ())
  in

  (* Compute common dependencies *)
  let common_exec, common_fndlb = 
    match lst with 
      | (_, hd) :: tl ->
          List.fold_left
            (fun (exec, fndlb) (_, (exec', fndlb')) ->
               SetExec.inter exec exec',
               SetFindlib.inter fndlb fndlb')
            hd tl
      | _ ->
          SetExec.empty,
          SetFindlib.empty
  in

  let debian_depends = 
    let cos = 
      OASISVersion.comparator_of_string 
    in
      List.fold_right 
        SetDepends.add 
        [
          "debhelper",     Some (cos ">= 7.0.50~"), `All;
          "dh-ocaml",      Some (cos ">= 0.9~"),    `All;
          "ocaml-findlib", pkg.findlib_version,     `All;

          "ocaml-nox",
          cmp_opt_merge
            pkg.ocaml_version
            (Some (cos ">= 3.11.1-3~")),
          `All;
        ]
        SetDepends.empty
  in

  let debian_depends = 
    debian_depends_of_depends 
      common_exec
      common_fndlb
      debian_depends
  in


  (* Check for extra dependencies for particular architecture *)
  let debian_depends = 
    List.fold_left 
      (fun st (arch_spec, (exec, fndlb)) ->
         let exec'  = SetExec.diff exec common_exec in
         let fndlb' = SetFindlib.diff fndlb common_fndlb in
           debian_depends_of_depends
             ~arch_spec
             exec'
             fndlb'
             st)
      debian_depends
      lst
  in

    (* Translate depends into string *)
    SetDepends.fold
      (fun dep lst ->
         string_of_depends dep :: lst)
      debian_depends

