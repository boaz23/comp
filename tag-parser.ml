#use "reader.ml";;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;


exception X_syntax_error;;

module type TAG_PARSER = sig
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;

(* work on the tag parser starts here *)
(*
--------------------------------------------------------------------------------
----------------------------------- Utilities ----------------------------------
--------------------------------------------------------------------------------
*)
let is_reserved_word = fun symbol -> List.mem symbol reserved_word_list;;

(*
--------------------------------------------------------------------------------
------------------------ S-Expr lists and pairs related ------------------------
------------- and convertion to OCaml list and pairs and vise versa ------------
--------------------------------------------------------------------------------
*)
let sexpr_pair_from_ocaml_list_with_remainder = fun (list, remainder) ->
  List.fold_right
    (fun item pairs -> Pair(item, pairs))
    list
    remainder;;

let sexpr_proper_list_from_ocaml_list = fun list ->
  sexpr_pair_from_ocaml_list_with_remainder (list, Nil);;

(*
  Converts a S-Expression pair to an OCaml pair (tuple with 2 items)
  of a list of all S-Expressions in the pairs (expect the last which is the cdr of the most inner pair)
  and the last S-Expresssion (remainder)
  Edge case: Nil. The function returns an empty list and Nil as the remainder
*)
let rec ocaml_list_with_remainder_from_sexpr_pair = function
  | Nil -> ([], Nil)
  | Pair(car, cdr) ->
    (match cdr with
    | Pair _ ->
      let (rest, last) = ocaml_list_with_remainder_from_sexpr_pair cdr in
      (car :: rest, last)
    | _ -> ([car], cdr))
  | _ -> raise X_syntax_error;;

(*
  Converts a S-Expression proper list to an OCaml list.

  Exceptions:
    X_syntax_error: When the given S-Expression is not a proper list
*)
let rec ocaml_list_from_sexpr_proper_list = fun sexpr_list ->
  match (ocaml_list_with_remainder_from_sexpr_pair sexpr_list) with
  | (list, Nil) -> list
  | _ -> raise X_syntax_error;;

let ocaml_pairs_list_from_sexpr_bindings = fun sexpr_list ->
  let ocaml_list_of_sexpr_paris = ocaml_list_from_sexpr_proper_list sexpr_list in
  List.map
    (fun pair ->
      match pair with
      | Pair(car, Pair(cdr, Nil)) -> (car, cdr)
      | _  -> raise X_syntax_error)
    ocaml_list_of_sexpr_paris;;

let ocaml_lists_pair_from_sexpr_bindings = fun sexpr_list ->
  let ocaml_paris_list = ocaml_pairs_list_from_sexpr_bindings sexpr_list in
  List.split ocaml_paris_list;;

(*
--------------------------------------------------------------------------------
----------------------- S-Expr constructors and functions ----------------------
--------------------------------------------------------------------------------
*)
let extract_symbol_string = function
  | Symbol(symbol) -> symbol
  | _ -> raise X_syntax_error;;

let make_list_sexpr_from = fun exp ->
  Pair(exp, Nil)

let make_var_sexpr = fun var_name ->
  Symbol(var_name);;

let make_app_sexpr = fun operator operands ->
  Pair(operator, operands);;
let make_operands_sexpr = fun operands ->
  sexpr_proper_list_from_ocaml_list operands;;

let make_lambda_sexpr = fun args body ->
  Pair(Symbol "lambda", Pair(args, body));;
let make_body_sexpr = fun exps ->
  sexpr_proper_list_from_ocaml_list exps;;
let make_args_sexpr = fun arg_names ->
  sexpr_proper_list_from_ocaml_list arg_names;;
let make_delayed_sexpr = fun exp ->
  make_lambda_sexpr Nil (make_body_sexpr [exp]);;

let make_begin_sexpr = fun exps ->
  Pair(Symbol "begin", exps);;

let make_set_sexpr_raw = fun var exp ->
  Pair(Symbol "set!", Pair(var, Pair(exp, Nil)));;
let make_set_sexpr = fun var_name exp ->
  make_set_sexpr_raw (make_var_sexpr var_name) exp;;

let make_if_sexpr_raw = fun test dit dif ->
  Pair(Symbol "if", Pair(test, Pair(dit, dif)));;
let make_if_sexpr = fun test dit dif ->
  Pair(Symbol "if", Pair(test, Pair(dit, Pair(dif, Nil))));;

let make_define_sexpr = fun var_name exp ->
  Pair(Symbol "define", Pair(make_var_sexpr var_name, Pair(exp, Nil)));;

let make_binding_sexpr_raw = fun var_sexpr value_exp ->
  sexpr_proper_list_from_ocaml_list [var_sexpr; value_exp];;
let make_binding_sexpr = fun var_name value_exp ->
  make_binding_sexpr_raw (make_var_sexpr var_name) value_exp;;
let make_delayed_binding_sexpr = fun var_name value_exp ->
  make_binding_sexpr var_name (make_delayed_sexpr value_exp);;
let make_bindings_sexpr = fun bindings_list ->
  sexpr_proper_list_from_ocaml_list bindings_list;;
let make_named_let_sexpr = fun name bindings body ->
  Pair(Symbol name, Pair(bindings, body));;
let make_let_sexpr = fun bindings body ->
  make_named_let_sexpr "let" bindings body;;

let make_quote_sexpr = fun sexpr ->
  Pair(Symbol "quote", Pair(sexpr, Nil));;

(*
--------------------------------------------------------------------------------
-------------------------------- Macro expanders -------------------------------
--------------------------------------------------------------------------------
*)
let expand_and_macro = fun exps ->
  match exps with
  | Nil -> Bool(true)
  | Pair(exp, Nil) -> exp
  | Pair(car, cdr) -> make_if_sexpr car (Pair(Symbol "and", cdr)) (Bool false)
  | _ -> raise X_syntax_error;;

let expand_mit_define_macro = fun var_name args body ->
  make_define_sexpr var_name (make_lambda_sexpr args body);;

let expend_pset_macro = fun bindings ->
  match bindings with
  (* no bindings *)
  | Nil -> Const Void

  (* exactly 1 binding, equivalent to a set! *)
  | Pair(Pair(var_sexpr, value_exp), Nil) -> make_set_sexpr_raw var_sexpr value_exp

  (* at least 2 bindings *)
  | Pair(Pair(var_sexpr, value_exp), rest_bindings) ->
    let var_name = extract_symbol_string var_sexpr in
    let var_prev_value_var_name = var_name ^ "-prev-value" in
    let var_new_value_var_name = var_name ^ "-new-value" in
    let rest_pset_var_name = "pset-rest-" ^ var_name in

    let bindings =
      let var_prev_value_binding = make_binding_sexpr var_prev_value_var_name var_sexpr in
      let var_new_value_binding = make_binding_sexpr var_new_value_var_name value_exp in
      let rest_pset_delayed_binding =
        let rest_pset_lambda =
          let args = make_args_sexpr [var_sexpr] in
          let body =
            let rest_pset_exp = Pair(Symbol "pset!", rest_bindings) in
            make_body_sexpr [rest_pset_exp] in
          make_lambda_sexpr args body in
        make_binding_sexpr rest_pset_var_name rest_pset_delayed_binding in
      make_bindings_sexpr [
        var_prev_value_binding;
        var_new_value_binding;
        rest_pset_delayed_binding
      ] in

    let body =
      let assign_var_set_exp = make_set_sexpr_raw var_sexpr (make_var_sexpr var_new_value_var_name) in
      let apply_rest_pset =
        let rest_pset_var_sexpr = make_var_sexpr rest_pset_var_name in
        let var_prev_value_operand = make_operands_sexpr [make_var_sexpr var_prev_value_var_name] in
        make_app_sexpr rest_pset_var_sexpr var_prev_value_operand in
      make_body_sexpr [
        assign_var_set_exp;
        apply_rest_pset
      ] in

    make_let_sexpr bindings body

  (* invalid syntax, covered for completeness and for clean compile without warnings *)
  | _ -> raise X_syntax_error;;

let expand_let_macro = fun bindings body ->
  let (var_sexprs_list, value_exps_list) = ocaml_lists_pair_from_sexpr_bindings bindings in
  let var_sexprs = make_args_sexpr var_sexprs_list in
  let value_exps = make_body_sexpr value_exps_list in
  let lambda = make_lambda_sexpr var_sexprs body in
  make_app_sexpr lambda value_exps;;

let expand_let_star_macro = fun bindings body ->
  match bindings with
  (* 0 or 1 variable bindings *)
  | (Nil | Pair(_, Nil)) -> make_let_sexpr bindings body

  (* at least 2 bindings *)
  | Pair(first_binding, rest_bindings) ->
    let bindings = make_bindings_sexpr [first_binding] in
    let innet_let_star = make_named_let_sexpr "let*" rest_bindings body in
    let body = make_body_sexpr [innet_let_star] in
    make_let_sexpr bindings body

  | _ -> raise X_syntax_error;;

let expand_letrec_macro = fun bindings body ->
  let (var_sexprs, value_exps) = ocaml_lists_pair_from_sexpr_bindings bindings in

  let bindings =
    let bindings_list = List.map
      (fun var_sexpr -> make_binding_sexpr_raw var_sexpr (make_quote_sexpr (Symbol "whatever")))
      var_sexprs in
    make_bindings_sexpr bindings_list in

  let body = (
    let assignments_exps = List.map2
      (fun var_sexpr value_exp -> make_set_sexpr_raw var_sexpr value_exp)
      var_sexprs
      value_exps in
    sexpr_pair_from_ocaml_list_with_remainder (assignments_exps, body)
  ) in
  make_let_sexpr bindings body;;

let expand_cond_macro = fun ribs ->
  let (ribs, _) = (
    (* filter out ribs after the else rib *)

    let ribs = ocaml_list_from_sexpr_proper_list ribs in
    List.fold_left
      (fun (ribs_acc, after_else_rib) rib ->
        match rib with
        | Pair(Symbol "else", _) -> (ribs_acc @ [rib], true)
        | _ ->
          let ribs_acc =
            if after_else_rib then ribs_acc
            else ribs_acc @ [rib] in
          (ribs_acc, false))
      ([], false)
      ribs
  ) in

  let (expansion, _) =
    let expand_rib_else = fun body -> make_begin_sexpr body in

    let expand_rib_normal = fun test body rest_ribs_expansion is_last_rib ->
      let dit = make_begin_sexpr body in
      let dif =
        if is_last_rib then Nil
        else make_list_sexpr_from rest_ribs_expansion in
      make_if_sexpr_raw test dit dif in

    let expand_rib_arrow = fun exp exp_f rest_ribs_expansion is_last_rib ->
      let bindings =
        let test_binding = make_binding_sexpr "value" exp in
        let f_binding = make_delayed_binding_sexpr "f" exp_f in
        let bindings_list =
          let bindings_list = [test_binding; f_binding] in
          if is_last_rib then bindings_list
          else
            let rest_binding = make_delayed_binding_sexpr "rest" rest_ribs_expansion in
            bindings_list @ [rest_binding] in
        make_bindings_sexpr bindings_list in

      let body =
        let test = make_var_sexpr "value" in
        let dit = make_app_sexpr
          (make_app_sexpr (make_var_sexpr "f") Nil)
          (make_list_sexpr_from (make_var_sexpr "value")) in
        let dif =
          if is_last_rib then Nil
          else make_list_sexpr_from (make_app_sexpr (make_var_sexpr "rest") Nil) in
        let inner_if = make_if_sexpr_raw test dit dif in
        make_body_sexpr [inner_if] in
      make_let_sexpr bindings body in

    List.fold_right
      (fun rib (rest_ribs_expansion, is_last_rib) ->
        match rib with
        | Pair(Symbol("else"), body) -> (expand_rib_else body, false)

        | Pair(exp, Pair(Symbol("=>"), Pair(exp_f, Nil))) ->
          (expand_rib_arrow exp exp_f rest_ribs_expansion is_last_rib, false)

        | Pair(test, body) ->
          (expand_rib_normal test body rest_ribs_expansion is_last_rib, false)

        | _ -> raise X_syntax_error)
      ribs
      (Nil, true) in
  expansion;;

let rec expand_quasiquote_macro = fun sexpr ->
  match sexpr with
  | (String(_)
     | Bool(_)
     | Number(_)
     | Char(_)) -> sexpr

  | Pair(Symbol("unquote"), Pair(sexpr, Nil)) -> sexpr
  | Pair(Symbol("unquote-splicing"), Pair(_, Nil)) -> make_quote_sexpr sexpr
  | (Nil | Symbol(_)) -> make_quote_sexpr sexpr

  | Pair(Pair(Symbol("unquote-splicing"), Pair(sexpr, Nil)), cdr) ->
    sexpr_proper_list_from_ocaml_list [
      Symbol "append";
      sexpr;
      expand_quasiquote_macro cdr
    ]

  | Pair(car, cdr) ->
    sexpr_proper_list_from_ocaml_list [
      Symbol "cons";
      expand_quasiquote_macro car;
      expand_quasiquote_macro cdr
    ];;

(*
--------------------------------------------------------------------------------
---------------------- Macro expansions pattern matching  ----------------------
--------------------------------------------------------------------------------
*)
let expand_macro_shallow = fun sexpr ->
  match sexpr with
  | Pair(Symbol("and"), exps) ->
    ((expand_and_macro exps), true)

  | Pair(Symbol("define"), Pair(Pair(Symbol(var_name), args), body)) ->
    ((expand_mit_define_macro var_name args body), true)

  | Pair(Symbol("let"), Pair(bindings, body)) ->
    ((expand_let_macro bindings body), true)

  | Pair(Symbol("let*"), Pair(bindings, body)) ->
    ((expand_let_star_macro bindings body), true)

  | Pair(Symbol("letrec"), Pair(bindings, body)) ->
    ((expand_letrec_macro bindings body), true)

  | Pair(Symbol("cond"), ribs) ->
    ((expand_cond_macro ribs), true)

  | Pair(Symbol("quasiquote"), Pair(sexpr, Nil)) ->
    ((expand_quasiquote_macro sexpr), true)

  | Pair(Symbol("pset!"), bindings) ->
    ((expend_pset_macro bindings), true)

  | _ -> (sexpr, false)

let rec expand_macro = fun sexpr ->
  let (sexpr, has_been_expanded) = expand_macro_shallow sexpr in
    if has_been_expanded then (expand_macro sexpr)
    else sexpr

(*
--------------------------------------------------------------------------------
------------------------------ Core forms parsers ------------------------------
--------------------------------------------------------------------------------
*)
let rec parse_const = fun sexpr -> Const(Sexpr sexpr)

and parse_var_form = fun symbol ->
  if not (is_reserved_word symbol)
  then Var symbol
  else raise X_syntax_error

and extract_var_name = fun smybol_sexpr ->
  let symbol_string = extract_symbol_string smybol_sexpr in
  let var = parse_var_form symbol_string
  match var with
  | Var var_name -> var_name

  (* cannot reach this case, here for completeness and in order to compile without warnings *)
  | _ -> raise X_syntax_error

and parse_if_form = fun test dit dif ->
  let test = tag_parse test in
  let dit = tag_parse dit in
  let dif =
    (match dif with
      | Nil -> Const Void
      | Pair(dif, Nil) -> tag_parse dif
      | _ -> raise X_syntax_error) in
  If(test, dit, dif)

and parse_def_form = fun var_name exp ->
  Def((parse_var_form var_name), (tag_parse exp))

and parse_set_form = fun var_name exp ->
  Set((parse_var_form var_name), (tag_parse exp))

and parse_or_form = fun exps ->
  let exprs = parse_exps_list exps in
  match exprs with
  | [] -> parse_const (Bool false)
  | expr :: [] -> expr
  | exprs -> Or exprs

and parse_appic_form = fun operator operands ->
  let operator_expr = tag_parse operator in
  let operands_exprs = parse_exps_list operands in
    Applic(operator_expr, operands_exprs)

and parse_begin_form = fun exps -> parse_sequence exps

and parse_lambda_form = fun args body_exps ->
  let body = parse_sequence body_exps in

  (* check for non empty body *)
  if body = Const Void then raise X_syntax_error (* empty body *)
  else (
    match args with

    (* an empty list (Simple) *)
    (* (lambda () ...) *)
    | Nil -> LambdaSimple([], body)

    (* a symbol (Opt - Variadic) *)
    (* (lambda c ...) *)
    | Symbol(variadic_arg) -> LambdaOpt([], variadic_arg, body)

    | Pair _ -> (
      let extract_arg_names = fun args -> List.map extract_var_name args in

      let has_duplicates = fun list ->
        let no_duplicates_list = List.sort_uniq String.compare list in
        let lengths_cmp_result = List.compare_lengths list no_duplicates_list in
        lengths_cmp_result <> 0 in

      (* start reading from here *)
      let (args, arg_remainder) = ocaml_list_with_remainder_from_sexpr_pair args in
      let args = extract_arg_names args in
      if (has_duplicates args) then raise X_syntax_error
      else match arg_remainder with

        (* an empty list *)
        (* args are a non-empty proper list (Simple) *)
        (* (lambda (a b c) ...) *)
        | Nil -> LambdaSimple(args, body)

        (* a symbol *)
        (* args are an improper list (Opt) *)
        (* (lambda (a b . c) ...) *)
        | Symbol(optional_arg) ->
          if (List.mem optional_arg args) then raise X_syntax_error
          else LambdaOpt(args, optional_arg, body)
        | _ -> raise X_syntax_error
      )
    | _ -> raise X_syntax_error
  )

and parse_exps_list = fun exps ->
  let sexprs_list = ocaml_list_from_sexpr_proper_list exps in
  List.fold_right
    (fun sexpr acc -> (tag_parse sexpr) :: acc)
    sexprs_list
    []

and parse_sequence = fun exps ->
  let exps = ocaml_list_from_sexpr_proper_list exps in
  let f_build_expr_list =
    (fun exp acc ->
      let expr = tag_parse exp in
      match expr with
      | Seq expr_list -> expr_list @ acc
      | expr -> expr :: acc) in
  let exprs = List.fold_right
      f_build_expr_list
      exps
      [] in
    match exprs with
    | [] -> Const Void
    | expr :: [] -> expr
    | exprs -> Seq exprs

(*
--------------------------------------------------------------------------------
-------------------------- Core forms pattern matching -------------------------
--------------------------------------------------------------------------------
*)
and parse_core_form = function
  | Nil -> raise X_syntax_error
  | (Bool(_)
     | Number(_)
     | Char(_)
     | String(_)) as sexpr -> parse_const sexpr
  | Pair(Symbol("quote"), Pair(sexpr, Nil)) -> parse_const sexpr
  | Symbol(symbol) -> parse_var_form symbol

  (* (if <test> <then> <else>) *)
  | Pair(Symbol("if"), Pair(test, Pair(dit, dif))) ->
    parse_if_form test dit dif

  (* (define <var> <exp>) *)
  | Pair(Symbol("define"), Pair(Symbol(var_name), Pair(exp, Nil))) ->
    parse_def_form var_name exp

  (* (set! <var> <exp>) *)
  | Pair(Symbol("set!"), Pair(Symbol(var_name), Pair(exp, Nil))) ->
    parse_set_form var_name exp

  (* (or <exp>* ) *)
  | Pair(Symbol("or"), exps) ->
    parse_or_form exps

  (* (begin <exp>* ) *)
  | Pair(Symbol("begin"), exps) ->
    parse_begin_form exps

  (* (lambda ( <arg>* ) <exp>+) *)
  | Pair(Symbol("lambda"), Pair(args, body)) ->
    parse_lambda_form args body

  (* application expression *)
  (* (<operator> <operands>* ) *)
  | Pair(operator, operands) ->
    parse_appic_form operator operands

(*
--------------------------------------------------------------------------------
--------------------------------- Tag-Parsing ----------------------------------
--------------------------------------------------------------------------------
*)
and tag_parse = fun sexpr -> parse_core_form (expand_macro sexpr);;

let tag_parse_expressions sexpr = List.map tag_parse sexpr;;

end;; (* struct Tag_Parser *)
