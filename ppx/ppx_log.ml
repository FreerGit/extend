open Ppxlib
open Base

let parse_format_placeholders format_str =
  let len = String.length format_str in
  let placeholders = Queue.create () in
  let i = ref 0 in
  while !i < len do
    if !i < len && Char.(format_str.[!i] = '%') && !i + 1 < len
    then
      if Char.(format_str.[!i + 1] = '%')
      then i := !i + 2
      else (
        let start = !i in
        Int.incr i;
        while
          !i < len
          && Char.(
               (format_str.[!i] >= 'a' && format_str.[!i] <= 'z')
               || (format_str.[!i] >= 'A' && format_str.[!i] <= 'Z')
               || (format_str.[!i] >= '0' && format_str.[!i] <= '9')
               || format_str.[!i] = '_')
        do
          Int.incr i
        done;
        let placeholder = String.sub format_str ~pos:start ~len:(!i - start) in
        Queue.enqueue placeholders placeholder)
    else Int.incr i
  done;
  Queue.to_array placeholders
;;

let printer_from_type ~loc core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr ({ txt = Lident type_name; _ }, []) ->
    let printer_name = "print_" ^ type_name in
    Ast_builder.Default.evar ~loc printer_name
  | Ptyp_constr ({ txt = Ldot (module_path, "t"); _ }, []) ->
    let printer_path = Ldot (module_path, "print") in
    Ast_builder.Default.pexp_ident ~loc (Loc.make ~loc printer_path)
  | Ptyp_constr ({ txt = Ldot (module_path, type_name); _ }, []) ->
    let printer_name = "print_" ^ type_name in
    let printer_path = Ldot (module_path, printer_name) in
    Ast_builder.Default.pexp_ident ~loc (Loc.make ~loc printer_path)
  | _ ->
    Location.raise_errorf
      ~loc
      "%%a requires type annotation like (expr : Type.t) or (expr : Module.t)"
;;

let wrap_arg ~loc placeholder expr =
  match placeholder with
  | "%d" | "%i" -> [%expr Logger.Arg (Logger.TInt, [%e expr])]
  | "%f" -> [%expr Logger.Arg (Logger.TFloat, [%e expr])]
  | "%b" -> [%expr Logger.Arg (Logger.TBool, [%e expr])]
  | "%s" -> [%expr Logger.Arg (Logger.TString, [%e expr])]
  | "%c" -> [%expr Logger.Arg (Logger.TChar, [%e expr])]
  | "%a" ->
    (match expr.pexp_desc with
     | Pexp_constraint (value_expr, core_type) ->
       let printer = printer_from_type ~loc core_type in
       [%expr Logger.Arg (Logger.TCustom [%e printer], [%e value_expr])]
     | _ ->
       Location.raise_errorf
         ~loc
         "%%a placeholder requires type annotation: use (expr : Type.t)")
  | _ -> Location.raise_errorf ~loc "Unknown placeholder: %s" placeholder
;;

(* Main extension *)
let log_extension =
  Extension.V3.declare
    "log"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt expr ->
       let loc = Expansion_context.Extension.extension_point_loc ctxt in
       let rec collect_args acc expr =
         match expr.pexp_desc with
         | Pexp_apply (fn, args) ->
           let arg_exprs = Array.of_list_map ~f:snd args in
           collect_args (Array.append arg_exprs acc) fn
         | _ -> expr, acc
       in
       let format_expr, args_exprs = collect_args [||] expr in
       let format_str =
         match format_expr.pexp_desc with
         | Pexp_constant (Pconst_string (s, _, _)) -> s
         | _ ->
           Location.raise_errorf
             ~loc
             "First argument to [%%log ...] must be a string literal"
       in
       let placeholders = parse_format_placeholders format_str in
       let placeholder_count = Array.length placeholders in
       let arg_count = Array.length args_exprs in
       if placeholder_count <> arg_count
       then
         Location.raise_errorf
           ~loc
           "Format string has %d placeholders but %d arguments provided"
           placeholder_count
           arg_count;
       let wrapped_args =
         Array.mapi
           ~f:(fun i placeholder -> wrap_arg ~loc placeholder args_exprs.(i))
           placeholders
       in
       match Array.length wrapped_args with
       | 0 -> [%expr Logger.Logger_Interal.log0 [%e format_expr]]
       | 1 -> [%expr Logger.Logger_Interal.log1 [%e format_expr] [%e wrapped_args.(0)]]
       | 2 ->
         [%expr
           Logger.Logger_Interal.log2
             [%e format_expr]
             [%e wrapped_args.(0)]
             [%e wrapped_args.(1)]]
       | 3 ->
         [%expr
           Logger.Logger_Interal.log3
             [%e format_expr]
             [%e wrapped_args.(0)]
             [%e wrapped_args.(1)]
             [%e wrapped_args.(2)]]
       | 4 ->
         [%expr
           Logger.Logger_Interal.log4
             [%e format_expr]
             [%e wrapped_args.(0)]
             [%e wrapped_args.(1)]
             [%e wrapped_args.(2)]
             [%e wrapped_args.(3)]]
       | 5 ->
         [%expr
           Logger.Logger_Interal.log5
             [%e format_expr]
             [%e wrapped_args.(0)]
             [%e wrapped_args.(1)]
             [%e wrapped_args.(2)]
             [%e wrapped_args.(3)]
             [%e wrapped_args.(4)]]
       | 6 ->
         [%expr
           Logger.Logger_Interal.log6
             [%e format_expr]
             [%e wrapped_args.(0)]
             [%e wrapped_args.(1)]
             [%e wrapped_args.(2)]
             [%e wrapped_args.(3)]
             [%e wrapped_args.(4)]
             [%e wrapped_args.(5)]]
       | _ ->
         let args_array =
           Ast_builder.Default.pexp_array ~loc (Array.to_list wrapped_args)
         in
         [%expr Logger.log [%e format_expr] [%e args_array]])
;;

let () = Driver.register_transformation "ppx_log" ~extensions:[ log_extension ]
