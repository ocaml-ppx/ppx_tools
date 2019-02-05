open Ast_mapper

let ppxs = ref []
let register name f =
  ppxs := (name, f) :: !ppxs
let ppxs () = List.rev !ppxs

let () = Ast_mapper.register_function := register

let main args =
  let mappers = List.rev_map (fun (_name, f) -> f args) (ppxs ()) in
  let structure _self st =
    let map st m = m.structure m st in
    List.fold_left map st mappers
  in
  let signature _self sg =
    let map sg m = m.signature m sg in
    List.fold_left map sg mappers
  in
  { default_mapper with structure; signature }

let run () = run_main main
