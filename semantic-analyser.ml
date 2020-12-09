#use "tag-parser.ml";;

type var =
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of var * expr'
  | Def' of var * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq (Var'(var1)) (Var'(var2))) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;


exception X_syntax_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics : SEMANTICS = struct

let make_set_expr' = fun var expr' ->
  Set' (var, expr');;
let make_def_expr' = fun var expr' ->
  Def' (var, expr');;
let make_seq_expr' = fun expr'_list ->
  Seq' expr'_list;;
let make_or_expr' = fun expr'_list ->
  Or' expr'_list;;
let make_lambda_simple_expr' = fun arg_names body_expr' ->
  LambdaSimple' (arg_names, body_expr');;
let make_lambda_opt_expr' = fun arg_names opt_arg_name body_expr' ->
  LambdaOpt' (arg_names, opt_arg_name, body_expr');;

let annotate_lexical_addresses_helper = fun e ->
  let vars_map = Hashtbl.create 16 in
  let rec annotate_set_bang = fun depth var expr ->
    let var_expr' = ann_lex_addr_traversal depth var in
    match var_expr' with
    | Var' var ->
      let expr' = ann_lex_addr_traversal depth expr in
      Set' (var, expr')
    | _ -> raise X_syntax_error

  and annotate_def = fun depth var expr ->
    let var_expr' = ann_lex_addr_traversal depth var in
    match var_expr' with
    | Var' var ->
      let expr' = ann_lex_addr_traversal depth expr in
      Def' (var, expr')
    | _ -> raise X_syntax_error

  and annotate_if = fun depth test dit dif ->
    If' (
      (ann_lex_addr_traversal depth test),
      (ann_lex_addr_traversal depth dit),
      (ann_lex_addr_traversal depth dif)
    )

  and annotate_applic = fun depth expr_operator exprs_operands ->
    Applic' (
      (ann_lex_addr_traversal depth expr_operator),
      (annotate_expr_list depth exprs_operands)
    )

  and annotate_expr_list = fun depth exprs ->
    List.map (ann_lex_addr_traversal depth) exprs

  and annotate_var = fun depth var_name ->
    let last_var_def = Hashtbl.find_opt vars_map var_name in
    match last_var_def with
    | Some (var_depth, var_pos) ->
      if var_depth = depth then Var' (VarParam(var_name, var_pos))
      else Var' (VarBound(var_name, depth - var_depth - 1, var_pos))
    | None -> Var' (VarFree var_name)

  and annotate_lambda = fun factory depth arg_names expr ->
    let next_depth = depth + 1 in
    begin
      add_args_to_map next_depth arg_names;
      let expr' = ann_lex_addr_traversal next_depth expr in
      begin
        remove_args_from_map arg_names;
        factory expr'
      end;
    end;
  and annotate_lambda_simple = fun depth arg_names expr ->
    annotate_lambda
      (fun expr' -> LambdaSimple' (arg_names, expr'))
      depth
      arg_names
      expr
  and annotate_lambda_opt = fun depth req_arg_names opt_arg_name expr ->
    annotate_lambda
      (fun expr' -> LambdaOpt' (req_arg_names, opt_arg_name, expr'))
      depth
      (req_arg_names @ [opt_arg_name])
      expr
  and add_args_to_map = fun depth arg_names ->
    List.iteri
      (fun arg_pos arg_name -> Hashtbl.add vars_map arg_name (depth, arg_pos))
      arg_names
  and remove_args_from_map = fun arg_names ->
    List.iter
      (fun arg_name -> Hashtbl.remove vars_map arg_name)
      arg_names

  and ann_lex_addr_traversal = fun depth expr ->
    match expr with
    | Const sexpr -> Const' sexpr
    | Var var_name -> annotate_var depth var_name
    | If (test, dit, dif) -> annotate_if depth test dit dif
    | Seq exprs -> Seq' (annotate_expr_list depth exprs)
    | Set (var, expr) -> annotate_set_bang depth var expr
    | Def (var, expr) -> annotate_def depth var expr
    | Or exprs -> Or' (annotate_expr_list depth exprs)
    | LambdaSimple (arg_names, body_expr) -> annotate_lambda_simple depth arg_names body_expr
    | LambdaOpt (req_arg_names, opt_arg_name, body_expr) -> annotate_lambda_opt depth req_arg_names opt_arg_name body_expr
    | Applic (expr_operator, exprs_operands) -> annotate_applic depth expr_operator exprs_operands in

  ann_lex_addr_traversal 0 e;;

let annotate_tail_calls_helpr = fun e ->

  let rec annotate_if = fun test dit dif is_in_tp ->
    let annotate_test = annotate_traversal test false in
    let annotate_dit = annotate_traversal dit is_in_tp in
    let annotate_dif = annotate_traversal dif is_in_tp in
      If'(annotate_test, annotate_dit, annotate_dif)

  and annotate_list_not_in_tail_pos = fun expr'_list ->
    List.map (fun expr' -> annotate_traversal expr' false) expr'_list

  and annotate_list = fun expr'_list is_in_tp factory ->
    let rev_list = List.rev expr'_list in 
    match rev_list with
    | [] -> factory []
    | expr' :: rest -> 
        let annotate_rev_list = annotate_list_not_in_tail_pos rev_list in
        let annotate_last = annotate_traversal expr' is_in_tp in
        let annotate_list = List.rev (annotate_last :: annotate_rev_list) in
          factory annotate_list

  and annotate_assignment_expr' = fun var expr' factory ->
    factory var (annotate_traversal expr' false)

  and annotate_lambda = fun body_expr' factory ->
    factory (annotate_traversal body_expr' true)

  and annotate_lambda_simple = fun args_names body_expr' ->
    annotate_lambda body_expr' (fun annotated_body_expr' -> make_lambda_simple_expr' args_names annotated_body_expr')
  
  and annotate_lambda_opt = fun args_names arg_name_opt body_expr' ->
    annotate_lambda body_expr' (fun annotated_body_expr' -> make_lambda_opt_expr' args_names arg_name_opt annotated_body_expr')

  and annotate_applic = fun expr'_operator exprs'_operands is_in_tp ->
    let annotate_exprs'_operands = annotate_list_not_in_tail_pos exprs'_operands in
    let annotate_expr'_operator = annotate_traversal expr'_operator is_in_tp in
      if is_in_tp
      then ApplicTP'(annotate_expr'_operator, annotate_exprs'_operands)
      else Applic'(annotate_expr'_operator, annotate_exprs'_operands)

  and annotate_traversal = fun expr' is_in_tp ->
  match expr' with
  | Const' sexpr -> e
  | Var' var -> e
  | Box' var -> e
  | BoxGet' var -> e
  | BoxSet' (var, expr') -> e
  | If' (test, dit, dif) -> annotate_if test dit dif is_in_tp
  | Seq' expr'_list -> annotate_list expr'_list is_in_tp make_seq_expr'
  | Set' (var, expr') -> annotate_assignment_expr' var expr' make_set_expr'
  | Def' (var, expr') -> annotate_assignment_expr' var expr' make_def_expr'
  | Or' expr'_list -> annotate_list expr'_list is_in_tp make_or_expr'
  | LambdaSimple' (args_names, body_expr') -> annotate_lambda_simple args_names body_expr'
  | LambdaOpt' (args_names, arg_name_opt, body_expr') -> annotate_lambda_opt args_names arg_name_opt body_expr'
  | Applic' (expr'_operator, exprs'_operands) -> annotate_applic expr'_operator exprs'_operands is_in_tp
  | ApplicTP' (expr'_operator, exprs'_operands) -> raise X_syntax_error in
  
  annotate_traversal e false;;


let annotate_lexical_addresses e = annotate_lexical_addresses_helper e;;

let annotate_tail_calls e = raise X_not_yet_implemented;;

let box_set e = raise X_not_yet_implemented;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;

end;; (* struct Semantics *)