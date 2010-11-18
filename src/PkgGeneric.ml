
(** Translate the provided package into its generic counterpart
  * See {!Common.t.pkg_generic} for a definition of what section
  * will be kept.
  *)

open OASISTypes

let create ~ctxt expr pkg = 

  let eval = 
    Expr.choose 
      ~ctxt 
      expr 
      (`All (fun x y -> x || y))
  in

  let sections =
    List.fold_left
      (fun acc e ->
         match e with 
           | Library (_, bs, _)
           | Executable (_, bs, _) ->
               if eval bs.bs_build && eval bs.bs_install then
                 e :: acc
               else
                 acc

           | Doc (_, doc) ->
               if eval doc.doc_build && eval doc.doc_install then
                 e :: acc
               else
                 acc

           | Flag _ | Test _ | SrcRepo _ ->
               e :: acc)
      []
      pkg.sections
  in

    {pkg with sections = List.rev sections}
