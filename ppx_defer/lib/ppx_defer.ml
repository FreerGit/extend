open Ppxlib
open Ast_builder.Default

let contains_defer expr =
  let found = ref false in
  let scanner =
    object
      inherit Ast_traverse.iter as super

      method! expression e =
        (match e.pexp_desc with
         | Pexp_extension ({ txt = "defer"; _ }, _)
         | Pexp_extension ({ txt = "defer.if"; _ }, _) -> found := true
         | _ -> ());
        super#expression e
    end
  in
  scanner#expression expr;
  !found
;;

let wrap_scopes =
  object (self)
    inherit Ast_traverse.map as super
    val mutable current_push_var = None

    method! value_binding vb =
      let body = vb.pvb_expr in
      match body.pexp_desc with
      | Parsetree.Pexp_function (params, constraint_, function_body)
        when contains_defer body ->
        let loc = body.pexp_loc in
        let push_name = gen_symbol ~prefix:"__push" () in
        let old_push = current_push_var in
        current_push_var <- Some push_name;
        let wrapped_body =
          match function_body with
          | Pfunction_body fb_expr ->
            let transformed_expr = self#expression fb_expr in
            Pfunction_body
              [%expr
                Ppx_defer_runtime.with_defer (fun [%p pvar ~loc push_name] ->
                  [%e transformed_expr])]
          | Pfunction_cases (cases, loc, attr) ->
            let transformed_cases =
              List.map
                (fun case ->
                   let transformed_rhs = self#expression case.pc_rhs in
                   { case with
                     pc_rhs =
                       [%expr
                         Ppx_defer_runtime.with_defer (fun [%p pvar ~loc push_name] ->
                           [%e transformed_rhs])]
                   })
                cases
            in
            Pfunction_cases (transformed_cases, loc, attr)
        in
        current_push_var <- old_push;
        { vb with
          pvb_expr =
            { body with
              pexp_desc = Parsetree.Pexp_function (params, constraint_, wrapped_body)
            }
        }
      | _ -> { vb with pvb_expr = self#expression body }

    method! expression expr =
      let open Parsetree in
      match expr.pexp_desc with
      | Pexp_extension ({ txt = "defer"; _ }, payload) ->
        (match current_push_var with
         | None ->
           Location.raise_errorf ~loc:expr.pexp_loc "defer used outside of a function"
         | Some push_name ->
           let loc = expr.pexp_loc in
           (match payload with
            | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] ->
              [%expr [%e evar ~loc push_name] (fun () -> [%e e])]
            | _ -> Location.raise_errorf ~loc:expr.pexp_loc "Invalid defer payload"))
      | Pexp_extension ({ txt = "defer.if"; _ }, payload) ->
        (match current_push_var with
         | None ->
           Location.raise_errorf ~loc:expr.pexp_loc "defer.if used outside of a function"
         | Some push_name ->
           let loc = expr.pexp_loc in
           (match payload with
            | PStr
                [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple [ cond; body ]; _ }, _)
                  ; _
                  }
                ] ->
              [%expr
                if [%e cond] then [%e evar ~loc push_name] (fun () -> [%e body]) else ()]
            | _ -> Location.raise_errorf ~loc:expr.pexp_loc "Invalid defer.if payload"))
      | _ -> super#expression expr
  end
;;

let () =
  let mapper = wrap_scopes in
  Driver.register_transformation ~impl:mapper#structure "ppx_defer"
;;
