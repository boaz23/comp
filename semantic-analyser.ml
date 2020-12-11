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
  | Box'(VarFree v1), Box'(VarFree v2) -> String.equal v1 v2
  | Box'(VarParam (v1,mn1)), Box'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Box'(VarBound (v1,mj1,mn1)), Box'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxGet'(VarFree v1), BoxGet'(VarFree v2) -> String.equal v1 v2
  | BoxGet'(VarParam (v1,mn1)), BoxGet'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | BoxGet'(VarBound (v1,mj1,mn1)), BoxGet'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxSet'(VarFree v1,e1), BoxSet'(VarFree v2, e2) -> String.equal v1 v2 && (expr'_eq e1 e2)
  | BoxSet'(VarParam (v1,mn1), e1), BoxSet'(VarParam (v2,mn2),e2) -> String.equal v1 v2 && mn1 = mn2 && (expr'_eq e1 e2)
  | BoxSet'(VarBound (v1,mj1,mn1),e1), BoxSet'(VarBound (v2,mj2,mn2),e2) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2 && (expr'_eq e1 e2)
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

let compute_bound_var_depth = fun depth var_depth_offset ->
  depth - var_depth_offset - 1;;

let annotate_lexical_addresses_helper = fun e ->
  let vars_map = Hashtbl.create 16 in

  let rec annotate_set = fun depth var_expr value_expr ->
    let (var, value_expr') = annotate_assignment_expr' depth var_expr value_expr in
    Set' (var, value_expr')
  and annotate_def = fun depth var_expr value_expr ->
    let (var, value_expr') = annotate_assignment_expr' depth var_expr value_expr in
    Def' (var, value_expr')

  and annotate_assignment_expr' = fun depth var_expr value_expr ->
    let annotated_var_expr' = annotate_traversal depth var_expr in
    match annotated_var_expr' with
    | Var' var ->
      let value_expr' = annotate_traversal depth value_expr in
      (var, value_expr')
    | _ -> raise X_syntax_error

  and annotate_expr_list = fun depth exprs ->
    List.map (annotate_traversal depth) exprs

  and annotate_var = fun depth var_name ->
    let last_var_def = Hashtbl.find_opt vars_map var_name in
    match last_var_def with
    | Some (var_depth, var_pos) ->
      if var_depth = depth then Var' (VarParam(var_name, var_pos))
      else let depth_offset = compute_bound_var_depth depth var_depth in
      Var' (VarBound(var_name, depth_offset, var_pos))
    | None -> Var' (VarFree var_name)

  and annotate_lambda_simple = fun depth arg_names expr ->
    let expr' =  annotate_lambda depth arg_names expr in
    LambdaSimple' (arg_names, expr')
  and annotate_lambda_opt = fun depth req_arg_names opt_arg_name expr ->
    let arg_names = (req_arg_names @ [opt_arg_name]) in
    let expr' =  annotate_lambda depth arg_names expr in
    LambdaOpt' (req_arg_names, opt_arg_name, expr')

  and annotate_lambda = fun depth arg_names expr ->
    let next_depth = depth + 1 in
    begin
      add_args_to_map next_depth arg_names;
      let expr' = annotate_traversal next_depth expr in
      begin
        remove_args_from_map arg_names;
        expr'
      end;
    end;
  and add_args_to_map = fun depth arg_names ->
    List.iteri
      (fun arg_pos arg_name -> Hashtbl.add vars_map arg_name (depth, arg_pos))
      arg_names
  and remove_args_from_map = fun arg_names ->
    List.iter
      (fun arg_name -> Hashtbl.remove vars_map arg_name)
      arg_names

  and annotate_traversal = fun depth expr ->
    match expr with
    | Const sexpr -> Const' sexpr
    | Var var_name -> annotate_var depth var_name
    | If (test, dit, dif) -> If' (
        (annotate_traversal depth test),
        (annotate_traversal depth dit),
        (annotate_traversal depth dif)
      )
    | Seq exprs -> Seq' (annotate_expr_list depth exprs)
    | Set (var_expr, value_expr) -> annotate_set depth var_expr value_expr
    | Def (var_expr, value_expr) -> annotate_def depth var_expr value_expr
    | Or exprs -> Or' (annotate_expr_list depth exprs)
    | LambdaSimple (arg_names, body_expr) -> annotate_lambda_simple depth arg_names body_expr
    | LambdaOpt (req_arg_names, opt_arg_name, body_expr) -> annotate_lambda_opt depth req_arg_names opt_arg_name body_expr
    | Applic (expr_operator, exprs_operands) -> Applic' (
        (annotate_traversal depth expr_operator),
        (annotate_expr_list depth exprs_operands)
      ) in

  annotate_traversal 0 e;;

let annotate_tail_calls_helper = fun e ->
  let rec annotate_if = fun test dit dif is_in_tp ->
    let annotate_test = annotate_traversal test false in
    let annotate_dit = annotate_traversal dit is_in_tp in
    let annotate_dif = annotate_traversal dif is_in_tp in
    If'(annotate_test, annotate_dit, annotate_dif)

  and annotate_list_not_in_tail_pos = fun expr'_list ->
    List.map (fun expr' -> annotate_traversal expr' false) expr'_list

  and annotate_list = fun expr'_list is_in_tp ->
    let rev_list = List.rev expr'_list in
    match rev_list with
    | [] -> []
    | expr' :: rest ->
      let annotate_rev_list = annotate_list_not_in_tail_pos rest in
      let annotate_last = annotate_traversal expr' is_in_tp in
      let annotate_list = List.rev (annotate_last :: annotate_rev_list) in
      annotate_list

  and annotate_assignment_expr' = fun value_expr' ->
    annotate_traversal value_expr' false

  and annotate_lambda = fun body_expr' ->
    annotate_traversal body_expr' true
  and annotate_lambda_simple = fun arg_names body_expr' ->
    let annotated_body_expr' = annotate_lambda body_expr' in
    LambdaSimple' (arg_names, annotated_body_expr')
  and annotate_lambda_opt = fun req_arg_names opt_arg_name body_expr' ->
    let annotated_body_expr' = annotate_lambda body_expr' in
    LambdaOpt' (req_arg_names, opt_arg_name, annotated_body_expr')

  and annotate_applic = fun expr'_operator exprs'_operands is_in_tp ->
    let annotate_exprs'_operands = annotate_list_not_in_tail_pos exprs'_operands in
    let annotate_expr'_operator = annotate_traversal expr'_operator is_in_tp in
    if is_in_tp
    then ApplicTP' (annotate_expr'_operator, annotate_exprs'_operands)
    else Applic' (annotate_expr'_operator, annotate_exprs'_operands)

  and annotate_traversal = fun expr' is_in_tp ->
  match expr' with
  | Const' _ -> expr'
  | Var' _ -> expr'
  | Box' _ -> expr'
  | BoxGet' _ -> expr'
  | BoxSet' _ -> expr'
  | If' (test, dit, dif) -> annotate_if test dit dif is_in_tp
  | Seq' expr'_list -> Seq' (annotate_list expr'_list is_in_tp)
  | Set' (var, value_expr') -> Set' (var, (annotate_assignment_expr' value_expr'))
  | Def' (var, value_expr') -> Def' (var, (annotate_assignment_expr' value_expr'))
  | Or' expr'_list -> Or' (annotate_list expr'_list is_in_tp)
  | LambdaSimple' (arg_names, body_expr') -> annotate_lambda_simple arg_names body_expr'
  | LambdaOpt' (req_arg_names, opt_arg_name, body_expr') -> annotate_lambda_opt req_arg_names opt_arg_name body_expr'
  | Applic' (expr'_operator, exprs'_operands) -> annotate_applic expr'_operator exprs'_operands is_in_tp

  (* impossible case, were annotate right now,
     so if we got an ApplicTP', it's probably a bug *)
  | ApplicTP' (expr'_operator, exprs'_operands) -> raise X_syntax_error in

  annotate_traversal e false;;

let annotate_boxes_helper = fun e ->
  let rec annotated_boxes_traversal = fun pos_list expr' ->
    raise X_not_yet_implemented in
  let annotated_boxes = fun factory pos_list body_expr' ->
    raise X_not_yet_implemented in

  let find_var_access_var = fun depth var read_write ->
    match var with
    | VarParam (_, pos) -> [(depth, pos, None, read_write)]
    | VarBound (_, depth_offset, pos) ->
      let dest_depth = compute_bound_var_depth depth depth_offset in
      [(dest_depth, pos, None, read_write)]
    | VarFree _ -> [] in

  let rec find_var_access_unordered_list = fun depth expr'_list ->
    let var_access_list_list = List.map (find_var_accesses_traversal depth) expr'_list in
    List.flatten var_access_list_list

  and filter_ordered_var_accesses = fun depth var_access_list_list ->
    (* TODO actually implemented this when done with the rest *)
    List.flatten var_access_list_list

  and find_var_accesses_ordered_list = fun depth expr'_list ->
    let var_accesses_list_list = List.map (find_var_accesses_traversal depth) expr'_list in
    filter_ordered_var_accesses depth var_accesses_list_list

  and find_var_accesses_applic = fun depth operator operands_list ->
    let operator_var_access = find_var_accesses_traversal depth operator in
    let operand_var_access = find_var_access_unordered_list depth operands_list in
    filter_ordered_var_accesses depth [operator_var_access; operand_var_access]

  and find_vars_and_annotate_lambda = fun factory depth arg_names body_expr' ->
    (* 1. get all the var access in the body
       2. find the var access for this lambda
       3. filter out vars according to rules 1 and 2
       4. annotated boxes (i.e. annotate all access as box get/set and add a set box at the beginning)
       5. filter out all vars which are parameters of this lambda
       6. map the rest of the access to replace the highest lambda in which the access appear to be this lambda
       7. return the var access list *)
    let body_var_access = find_var_accesses_traversal (depth + 1) body_expr' in

    let (var_access, parent_lambdas_var_accesses) = List.partition
      (fun (dest_depth, _, _, _) -> dest_depth = depth)
      body_var_access in

    let var_access = List.filter
      (fun (_, _, highest_lambda_opt, read_write) ->
        List.exists
          (fun (_, _, other_highest_lambda_opt, other_read_write) ->
            read_write <> other_read_write &&
            match highest_lambda_opt, other_highest_lambda_opt with
            | Some (LambdaSimple _), Some (LambdaSimple _) -> highest_lambda_opt != other_highest_lambda_opt
            | Some (LambdaOpt _), Some (LambdaOpt _) -> highest_lambda_opt != other_highest_lambda_opt
            | _ -> true)
        var_access)
      var_access in

    let annotated_lambda_expr' =
      let pos_list = List.map (fun (_, pos, _, _) -> pos) var_access in
      annotated_boxes factory pos_list body_expr' in

    let parent_lambdas_var_accesses = List.map
      (fun (dest_depth, pos, _, read_write) ->
        (dest_depth, pos, annotated_lambda_expr', read_write))
      parent_lambdas_var_accesses in

    (annotated_lambda_expr', parent_lambdas_var_accesses)

  and find_vars_and_annotate_lambda_simple = fun depth arg_names body_expr' ->
    find_vars_and_annotate_lambda
      (fun annotated_body_expr' -> LambdaSimple' (arg_names, annotated_body_expr'))
      depth
      arg_names
      body_expr'

  and find_vars_and_annotate_lambda_opt = fun depth req_arg_names opt_arg_name body_expr' ->
    find_vars_and_annotate_lambda
      (fun annotated_body_expr' -> LambdaOpt' (req_arg_names, opt_arg_name, annotated_body_expr'))
      depth
      (req_arg_names @ [opt_arg_name])
      body_expr'

  and find_var_accesses_traversal = fun depth expr' ->
    let (_, var_accesses) = find_var_accesses_traversal_root depth expr' in
    var_accesses

  (* data structure for the return value of the recursio:
    (annotated_expr', [(depth, pos, highest_lambda option, read/write)])
    read - true
    write - false
  *)
  and find_var_accesses_traversal_root = fun depth expr' ->
    match expr' with
    | Const' _ -> (expr', [])
    | Var' var -> (expr', find_var_access_var depth var true)
    | If' (test, dit, dif) -> (expr', find_var_access_unordered_list depth [test; dit; dif])
    | Seq' expr'_list -> (expr', find_var_accesses_ordered_list depth expr'_list)
    | Set' (var, value_expr') -> (expr', find_var_access_var depth var false)
    | Def' (var, value_expr') -> (expr', find_var_accesses_traversal depth value_expr')
    | Or' expr'_list -> (expr', find_var_accesses_ordered_list depth expr'_list)
    | (  Applic' (expr'_operator, exprs'_operands)
       | ApplicTP' (expr'_operator, exprs'_operands)) -> (expr', find_var_accesses_applic depth expr'_operator exprs'_operands)

    (* returns with possibly annotated body *)
    | LambdaSimple' (arg_names, body_expr') -> find_vars_and_annotate_lambda_simple depth arg_names body_expr'
    | LambdaOpt' (req_arg_names, opt_arg_name, body_expr') -> find_vars_and_annotate_lambda_opt depth req_arg_names opt_arg_name body_expr'

  (* impossible case, were annotate right now,
     so if we got a box, it's probably a bug *)
  | Box' _ -> raise X_not_yet_implemented
  | BoxGet' _ -> raise X_not_yet_implemented
  | BoxSet' _ -> raise X_not_yet_implemented in

  let (annotated_expr', _) = find_var_accesses_traversal_root 0 e in
  annotated_expr'

let annotate_lexical_addresses e = annotate_lexical_addresses_helper e;;

let annotate_tail_calls e = annotate_tail_calls_helper e;;

let box_set e = annotate_boxes_helper e;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;

end;; (* struct Semantics *)