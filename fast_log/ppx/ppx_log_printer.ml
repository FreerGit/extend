open Ppxlib
open Base

let generate_printer ~loc type_name fields =
  let printer_name =
    if String.equal type_name "t" then "print" else "print_" ^ type_name
  in
  let fields_array = Array.of_list fields in
  let num_fields = Array.length fields_array in
  let print_field_exprs =
    Array.mapi fields_array ~f:(fun i field ->
      let field_name = field.pld_name.txt in
      let is_last = i = num_fields - 1 in
      let field_var = Ast_builder.Default.evar ~loc field_name in
      let field_name_str = Ast_builder.Default.estring ~loc field_name in
      let print_value =
        match field.pld_type.ptyp_desc with
        | Ptyp_constr ({ txt = Lident "int"; _ }, []) ->
          [%expr Buffer.add_string buf (Int.to_string [%e field_var])]
        | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
          [%expr Buffer.add_string buf [%e field_var]]
        | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
          [%expr Buffer.add_string buf (Float.to_string [%e field_var])]
        | Ptyp_constr ({ txt = Lident "bool"; _ }, []) ->
          [%expr Buffer.add_string buf (Bool.to_string [%e field_var])]
        | Ptyp_constr ({ txt = Lident "char"; _ }, []) ->
          [%expr Buffer.add_char buf [%e field_var]]
        | Ptyp_constr ({ txt = Lident type_name; _ }, []) ->
          (* Nested custom type - call its printer *)
          let nested_printer = "print_" ^ type_name in
          [%expr [%e Ast_builder.Default.evar ~loc nested_printer] buf [%e field_var]]
        | Ptyp_constr ({ txt = Ldot (module_path, "t"); _ }, []) ->
          (* Module.t -> Module.print *)
          let printer_path = Ldot (module_path, "print") in
          let printer_expr =
            Ast_builder.Default.pexp_ident ~loc (Loc.make ~loc printer_path)
          in
          [%expr [%e printer_expr] buf [%e field_var]]
        | _ -> [%expr Buffer.add_string buf "<complex>"]
      in
      let separator =
        if is_last then [%expr ()] else [%expr Buffer.add_string buf "; "]
      in
      [%expr
        Buffer.add_string buf [%e field_name_str];
        Buffer.add_string buf "=";
        [%e print_value];
        [%e separator]])
  in
  let print_body =
    let open_brace = [%expr Buffer.add_string buf "{"] in
    let close_brace = [%expr Buffer.add_string buf "}"] in
    let body =
      Array.fold_right print_field_exprs ~init:close_brace ~f:(fun expr acc ->
        [%expr
          [%e expr];
          [%e acc]])
    in
    [%expr
      [%e open_brace];
      [%e body]]
  in
  let record_pat_fields =
    Array.map fields_array ~f:(fun field ->
      let field_lid = Loc.make ~loc (Lident field.pld_name.txt) in
      let field_var = Ast_builder.Default.pvar ~loc field.pld_name.txt in
      field_lid, field_var)
  in
  let record_pattern =
    Ast_builder.Default.ppat_record ~loc (Array.to_list record_pat_fields) Closed
  in
  let type_constr =
    Ast_builder.Default.ptyp_constr ~loc (Loc.make ~loc (Lident type_name)) []
  in
  let buf_pat = Ast_builder.Default.ppat_var ~loc (Loc.make ~loc "buf") in
  let value_pat = Ast_builder.Default.ppat_var ~loc (Loc.make ~loc "value") in
  let printer_fun_expr =
    Ast_builder.Default.pexp_fun
      ~loc
      Nolabel
      None
      buf_pat
      (Ast_builder.Default.pexp_fun
         ~loc
         Nolabel
         None
         value_pat
         [%expr
           let [%p record_pattern] = (value : [%t type_constr]) in
           [%e print_body]])
  in
  [ [%stri let [%p Ast_builder.Default.pvar ~loc printer_name] = [%e printer_fun_expr]] ]
;;

(* Main deriver *)
let log_printer_derive =
  Deriving.add
    "log_printer"
    ~str_type_decl:
      (Deriving.Generator.make_noarg
       @@ fun ~loc ~path:_ (_rec_flag, type_decls) ->
       let type_decls_array = Array.of_list type_decls in
       Array.to_list
         (Array.concat_map type_decls_array ~f:(fun type_decl ->
            match type_decl.ptype_kind with
            | Ptype_record fields ->
              Array.of_list (generate_printer ~loc type_decl.ptype_name.txt fields)
            | _ ->
              let loc = type_decl.ptype_loc in
              Location.raise_errorf
                ~loc
                "log_printer can only be derived for record types")))
;;
