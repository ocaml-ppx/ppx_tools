(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)


(* Generate code to perform a deep copy of a type into another
   identical type (in another module).  Used to generate a first
   version of migration code between two versions of the same type,
   to be then patched manually to perform actual migration. *)

module Main : sig end = struct

  open Types
  open Asttypes
  open Ast_helper
  open Ast_convenience

  let selfcall m args = app (evar m) args

  (*************************************************************************)


  let env = Env.initial_safe_string

  let module_mapping = ref []

  let clean s =
    let s = Bytes.of_string s in
    for i = 0 to Bytes.length s - 1 do
      if Bytes.get s i = '.' then Bytes.set s i '_'
    done;
    Bytes.to_string s

  let print_fun s = "copy_" ^ clean s

  let printed = Hashtbl.create 16
  let meths = ref []

  let rec gen ty =
    if Hashtbl.mem printed ty then ()
    else let tylid = Longident.parse ty in
      let td =
        try Env.find_type (Env.lookup_type tylid env) env
        with Not_found ->
          Format.eprintf "** Cannot resolve type %s@." ty;
          exit 2
      in
      let prefix, local =
        let open Longident in
        match tylid with
        | Ldot (m, s) -> String.concat "." (Longident.flatten m) ^ ".", s
        | Lident s -> "", s
        | Lapply _ -> assert false
      in
      let target_prefix =
        try
          let (v1, v2) =
            List.find
              (fun (v1, v2) ->
                 String.length v1 <= String.length prefix
                 && String.sub prefix 0 (String.length v1) = v1
              )
              !module_mapping
          in
          v2 ^ String.sub prefix (String.length v1) (String.length prefix - String.length v1)
        with Not_found ->
          prefix
      in

      let funname = print_fun ty in
      Hashtbl.add printed ty ();
      let params_in = List.mapi (fun i _ -> Printf.sprintf "f%i" i) td.type_params in
      let params_out = List.mapi (fun i _ -> Printf.sprintf "g%i" i) td.type_params in

      let env = List.map2 (fun s t -> t.id, evar s) params_in td.type_params in
      let make_result_t tyargs_in tyargs_out =
        Typ.(arrow Asttypes.Nolabel
               (constr (lid (prefix ^ local)) tyargs_in)
               (constr (lid (target_prefix ^ local)) tyargs_out)
            )
      in
      let make_t tyargs_in tyargs_out =
        List.fold_right2
          (fun arg_in arg_out t ->
             Typ.(arrow Asttypes.Nolabel (arrow Asttypes.Nolabel arg_in arg_out) t))
          tyargs_in tyargs_out (make_result_t tyargs_in tyargs_out)
      in

      let tyargs_in = List.map (fun t -> Typ.var t) params_in in
      let tyargs_out = List.map (fun t -> Typ.var t) params_out in
      let t = Typ.poly (params_in @ params_out) (make_t tyargs_in tyargs_out) in

      let concrete e =
        let e = List.fold_right (fun x e -> lam x e) (List.map (fun x -> pvar x) params_in) e in
        let e = List.fold_right (fun x e -> Exp.newtype x e) (params_in @ params_out) e in
        let e = Exp.constraint_ e t in
        meths := Vb.mk (pvar funname) e :: !meths
      in
      let field ld =
        let s = Ident.name ld.ld_id in
        (lid (prefix ^ s), pvar s),
        (lid (target_prefix ^ s), tyexpr env ld.ld_type (evar s))
      in
      match td.type_kind, td.type_manifest with
      | Type_record (l, _), _ ->
          let l = List.map field l in
          concrete
            (lam
               (Pat.record (List.map fst l) Closed)
               (Exp.record (List.map snd l) None)
            )
      | Type_variant l, _ ->
          let case cd =
            let c = Ident.name cd.cd_id in
            match cd.cd_args with
            | Cstr_tuple tys ->
                let p, args = gentuple env tys in
                pconstr (prefix ^ c) p,
                constr (target_prefix ^ c) args
            | Cstr_record _l ->
                failwith "Inline records are not yet supported."
          in
          concrete (func (List.map case l))
      | Type_abstract, Some t ->
          concrete (tyexpr_fun env t)
      | Type_abstract, None ->
          failwith "Abstract type."
      | Type_open, _ ->
          failwith "Open types are not yet supported."

  and gentuple env tl =
    let arg i t =
      let x = Printf.sprintf "x%i" i in
      pvar x, tyexpr env t (evar x)
    in
    List.split (List.mapi arg tl)

  and tyexpr env ty x =
    match ty.desc with
    | Tvar _ ->
        (match List.assoc ty.id env with
         | f -> app f [x]
         | exception Not_found ->
             failwith "Existentials not supported")
    | Ttuple tl ->
        let p, e = gentuple env tl in
        let_in [Vb.mk (Pat.tuple p) x] (tuple e)
    | Tconstr (path, [t], _) when Path.same path Predef.path_list ->
        app (evar "List.map") [tyexpr_fun env t; x]
    | Tconstr (path, [t], _) when Path.same path Predef.path_array ->
        app (evar "Array.map") [tyexpr_fun env t; x]
    | Tconstr (path, [], _) when Path.same path Predef.path_string
                                 || Path.same path Predef.path_int
                                 || Path.same path Predef.path_char
                                 || Path.same path Predef.path_int32
                                 || Path.same path Predef.path_int64
                                 || Path.same path Predef.path_nativeint ->
        x

    | Tconstr (path, tl, _) ->
        let ty = Path.name path in
        gen ty;
        selfcall (print_fun ty) (List.map (tyexpr_fun env) tl @ [x])
    | _ ->
        Format.eprintf "** Cannot deal with type %a@." Printtyp.type_expr ty;
        exit 2

  and tyexpr_fun env ty =
    lam (pvar "x") (tyexpr env ty (evar "x"))

  let simplify =
    (* (fun x -> <expr> x) ====> <expr> *)
    let open Ast_mapper in
    let super = default_mapper in
    let expr this e =
      let e = super.expr this e in
      let open Longident in
      let open Parsetree in
      match e.pexp_desc with
      | Pexp_fun
          (Asttypes.Nolabel, None,
           {ppat_desc = Ppat_var{txt=id;_};_},
           {pexp_desc =
              Pexp_apply
                (f,
                 [Asttypes.Nolabel
                 ,{pexp_desc= Pexp_ident{txt=Lident id2;_};_}]);_})
        when id = id2 -> f
      | _ -> e
    in
    {super with expr}

  let add_mapping s =
    let i =
      try String.index s ':'
      with Not_found ->
        failwith (Printf.sprintf "Cannot parse mapping %S" s)
    in
    module_mapping :=
      (String.sub s 0 i ^ ".",
       String.sub s (i + 1) (String.length s - i - 1) ^ ".") :: !module_mapping

  let args =
    let open Arg in
    [
      "-I", String (fun s -> Config.load_path := Misc.expand_directory Config.standard_library s :: !Config.load_path),
      "<dir> Add <dir> to the list of include directories";
      "-map", String add_mapping,
      "Old_module:New_module  Map types from Old_module to types in New_module";
    ]

  let usage =
    Printf.sprintf "%s [options] <type names>\n" Sys.argv.(0)

  let main () =
    Config.load_path := [Config.standard_library];
    Arg.parse (Arg.align args) gen usage;
    let s = [Str.value Recursive !meths] in
    Format.printf "%a@." Pprintast.structure (simplify.Ast_mapper.structure simplify s)

  let () =
    try main ()
    with exn ->
      Printf.eprintf "** fatal error: %s\n%!" (Printexc.to_string exn)

end
