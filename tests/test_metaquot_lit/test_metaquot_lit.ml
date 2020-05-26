let () =
  match [%expr [%lit.integer "10"]] with
  | { pexp_desc = Pexp_constant (Pconst_integer ("10", None)); _ } -> ()
  | _ -> assert false

let () =
  match Ast_helper.Exp.constant (Ast_helper.Const.integer "10") with
  | [%expr [%lit.integer? "0"]] -> assert false
  | [%expr [%lit.integer? "10"]] -> ()
  | _ -> assert false

let () =
  match [%expr [%lit.integer "10"] [@suffix Some 'l']] with
  | { pexp_desc = Pexp_constant (Pconst_integer ("10", Some 'l')); _ } -> ()
  | _ -> assert false

let () =
  match
    Ast_helper.Exp.constant (Ast_helper.Const.integer "10"  ~suffix:'l')
  with
  | [%expr [%lit.integer? "10"] [@suffix? None]] -> assert false
  | [%expr [%lit.integer? "10"] [@suffix? Some 'l']] -> ()
  | _ -> assert false

let () =
  match [%expr [%lit.char 'c']] with
  | { pexp_desc = Pexp_constant (Pconst_char 'c'); _ } -> ()
  | _ -> assert false

let () =
  match Ast_helper.Exp.constant (Ast_helper.Const.char 'c') with
  | [%expr [%lit.char? 'a']] -> assert false
  | [%expr [%lit.char? 'c']] -> ()
  | _ -> assert false

let () =
  match [%expr [%lit.string "s"]] with
  | { pexp_desc = Pexp_constant (Pconst_string ("s", _, None)); _ } -> ()
  | _ -> assert false

let () =
  match Ast_helper.Exp.constant (Ast_helper.Const.string "s") with
  | [%expr [%lit.string? ""]] -> assert false
  | [%expr [%lit.string? "s"]] -> ()
  | _ -> assert false

let () =
  match [%expr [%lit.string "s"] [@quotation_delimiter Some "t"]] with
  | { pexp_desc = Pexp_constant (Pconst_string ("s", _, Some "t")); _ } -> ()
  | _ -> assert false

let () =
  match
    Ast_helper.Exp.constant
      (Ast_helper.Const.string ~quotation_delimiter:"t" "s") with
  | [%expr [%lit.string? "s"] [@quotation_delimiter? None]] -> assert false
  | [%expr [%lit.string? "s"] [@quotation_delimiter? Some "t"]] -> ()
  | _ -> assert false

let () =
  match [%expr [%lit.float "1.0"]] with
  | { pexp_desc = Pexp_constant (Pconst_float ("1.0", None)); _ } -> ()
  | _ -> assert false

let () =
  match Ast_helper.Exp.constant (Ast_helper.Const.float "1.0") with
  | [%expr [%lit.float? "0.0"]] -> assert false
  | [%expr [%lit.float? "1.0"]] -> ()
  | _ -> assert false

let () =
  match [%expr [%lit.float "1.0"] [@suffix Some 'f']] with
  | { pexp_desc = Pexp_constant (Pconst_float ("1.0", Some 'f')); _ } -> ()
  | _ -> assert false

let () =
  match Ast_helper.Exp.constant (Ast_helper.Const.float "1.0" ~suffix:'f') with
  | [%expr [%lit.float? "1.0"] [@suffix? None]] -> assert false
  | [%expr [%lit.float? "1.0"] [@suffix? Some 'f']] -> ()
  | _ -> assert false
