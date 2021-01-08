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
let rec sexpr_pair_fold_left f_list f_remainder acc sexpr =
  match sexpr with
  | Nil -> acc
  | Pair (car, cdr) -> sexpr_pair_fold_left f_list f_remainder (f_list acc car) cdr
  | _ -> f_remainder acc sexpr;;

let sexpr_char_to_string = function
  | '\000' -> "nul"
  | '\n' -> "newline"
  | '\012' -> "page"
  | '\r' -> "return"
  | ' ' -> "space"
  | '\t' -> "tab"
  | c -> String.make 1 c;;

let sexpr_string_to_string = fun s ->
  let chars_list = string_to_list s in
  let chars_list = List.fold_right
    (fun c acc ->
      let l =
      match c with
      | '\\' -> ['\\'; '\\']
      | '"' -> ['\\'; '"']
      | '\t' -> ['\\'; 't']
      | '\012' -> ['\\'; 'f']
      | '\n' -> ['\\'; 'n']
      | '\r' -> ['\\'; 'r']
      | _ -> [c] in
      l @ acc)
    chars_list
    [] in
  list_to_string chars_list

let rec sexpr_pair_to_string = fun pair ->
  let f_pair_inner = fun (acc, is_first) sexpr ->
    let sexpr_string = sexpr_to_string sexpr in
    let new_acc =
    if is_first then sexpr_string
    else Printf.sprintf "%s %s" acc sexpr_string in
    (new_acc, false) in

  let f_remainder = fun (acc, is_first) sexpr ->
    let sexpr_string = sexpr_to_string sexpr in
    let new_acc = Printf.sprintf "%s . %s" acc sexpr_string in
    (new_acc, is_first) in

  let (string, _) = sexpr_pair_fold_left f_pair_inner f_remainder ("", true) pair in
    Printf.sprintf "(%s)" string

and sexpr_to_string = function
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Nil -> "'()"
  | Number (Fraction (n, d)) when d = 1 -> string_of_int n
  | Number (Fraction(n, d)) -> Printf.sprintf "%d/%d" n d
  | Number (Float f) -> string_of_float f
  | Char c -> Printf.sprintf "#\\%s" (sexpr_char_to_string c)
  | String s -> Printf.sprintf "\"%s\"" s
  | Symbol s -> s
  | Pair(Symbol "quote", Pair(sexpr, Nil)) -> Printf.sprintf "'%s" (sexpr_to_string sexpr)
  | (Pair _ as pair) -> sexpr_pair_to_string pair;;

let constant_to_string = function
  | Sexpr sexpr -> sexpr_to_string sexpr
  | Void -> "Void";;

let var_to_string = fun var ->
  match var with
  | VarFree var_name -> Printf.sprintf "VarFree \"%s\"" var_name
  | VarParam (var_name, var_pos) -> Printf.sprintf "VarParam (\"%s\", %d))" var_name var_pos
  | VarBound (var_name, major, minor) -> Printf.sprintf "VarBound (\"%s\", %d, %d)" var_name major minor;;

let rec expr'_list_to_string = fun expr's_list ->
  let (s, _) =
    List.fold_right
      (fun expr' (acc, is_last) ->
        let expr'_string = expr'_to_string expr' in
        let new_acc =
          if is_last then expr'_string
          else Printf.sprintf "%s; %s" expr'_string acc in
        (new_acc, false))
      expr's_list
      ("", true) in
  Printf.sprintf "[%s]" s

and lambda_to_stirng = fun lambda_name args_list arg_opt expr' ->
  let args_list = List.map (fun s -> "\"" ^ s ^ "\"") args_list in
  let args_list_s = String.concat "; " args_list in
  let args_list_s = "[" ^ args_list_s ^ "]" in
  let expr'_s = expr'_to_string expr' in
  let arg_opt_s =
    if arg_opt <> ""
    then ", \"" ^ arg_opt ^ "\""
    else "" in
  Printf.sprintf "%s (%s%s, (%s))" lambda_name args_list_s arg_opt_s expr'_s

and applic_to_string = fun applic_name expr' expr'_list ->
  let expr'_s = expr'_to_string expr' in
  let expr'_list_s = expr'_list_to_string expr'_list in
  Printf.sprintf "%s (%s, %s)" applic_name expr'_s expr'_list_s

and expr'_to_string = function
  | Const' const -> Printf.sprintf "Const' %s" (constant_to_string const)
  | Var' var -> Printf.sprintf "Var' (%s)" (var_to_string var)
  | Box' var -> Printf.sprintf "Box' (%s)" (var_to_string var)
  | BoxGet' var -> Printf.sprintf "BoxGet' (%s)" (var_to_string var)
  | BoxSet' (var, expr') -> Printf.sprintf "BoxSet' ((%s), (%s))" (var_to_string var) (expr'_to_string expr')
  | If' (test, dit, dif) ->
    let s_test = expr'_to_string test in
    let s_dit = expr'_to_string dit in
    let s_dif = expr'_to_string dif in
    Printf.sprintf "(If' ((%s), (%s), (%s)))" s_test s_dit s_dif
  | Seq' expr'_list -> Printf.sprintf "(Seq' %s)" (expr'_list_to_string expr'_list)
  | Set' (var, expr') -> Printf.sprintf "(Set' ((%s), (%s)))" (var_to_string var) (expr'_to_string expr')
  | Def' (var, expr') -> Printf.sprintf "(Def' ((%s), (%s)))" (var_to_string var) (expr'_to_string expr')
  | Or' expr'_list -> Printf.sprintf "(Or' %s)" (expr'_list_to_string expr'_list)

  | LambdaSimple' (args_list, expr') -> lambda_to_stirng "LambdaSimple'" args_list "" expr'
  | LambdaOpt' (args_list, arg_opt, expr') -> lambda_to_stirng "LambdaOpt'" args_list arg_opt expr'
  | Applic' (expr', expr'_list) -> applic_to_string "Applic'" expr' expr'_list
  | ApplicTP' (expr', expr'_list) -> applic_to_string "ApplicTP'" expr' expr'_list;;

let print_var_access = fun var_access ->
  match var_access with
    | (depth, pos, highest_lambda_opt, read_write) ->
      let s =
        match highest_lambda_opt with
        | Some lambda_expr' -> expr'_to_string lambda_expr'
        | None -> "" in
        Printf.printf "(%d, %d, %b, %s)" depth pos read_write s;;
let print_var_accesses = fun var_accesses ->
  match var_accesses with
  | [] -> Printf.printf "[]\n"
  | first :: rest ->
    begin
      Printf.printf "[\n";
      print_var_access first;
      Printf.printf ";\n ";
      List.iter
        (fun var_access ->
          print_var_access var_access;
          Printf.printf ";\n ")
        rest;
      Printf.printf "]\n"
    end;;

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
      let depth_offset = compute_bound_var_depth depth var_depth in
      if depth_offset = -1 then Var' (VarParam(var_name, var_pos))
      else if depth >= 0 then Var' (VarBound(var_name, depth_offset, var_pos))
      else raise (Failure "Lexical addressing: negative depth of less than -1")
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
    | Applic (operator_expr, operands_expr_list) -> Applic' (
        (annotate_traversal depth operator_expr),
        (annotate_expr_list depth operands_expr_list)
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

  and annotate_applic = fun operator_expr' operands_expr'_list is_in_tp ->
    let annotate_expr'_operator = annotate_traversal operator_expr' is_in_tp in
    let annotate_exprs'_operands = annotate_list_not_in_tail_pos operands_expr'_list in
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
  | Applic' (operator_expr', operands_expr'_list) -> annotate_applic operator_expr' operands_expr'_list is_in_tp

  (* impossible case, were annotate right now,
     so if we got an ApplicTP', it's probably a bug *)
  | ApplicTP' (expr'_operator, exprs'_operands) -> raise X_syntax_error in

  annotate_traversal e false;;

let annotate_boxes_helper = fun e ->
  let box_arguments = fun factory annotation_args body_expr' ->
    let annotate_box_var_pos = fun factory box_factory depth args_pos_list var_pos var ->
      if depth = 1 then (* the var references a parameter lambda which needs boxing annotation (calling lambda) *)
        let annotation_arg_opt = List.find_opt
            (fun arg_pos -> arg_pos = var_pos)
            args_pos_list in
        match annotation_arg_opt with
        | Some _ -> box_factory var
        | None -> factory var
      else if depth > 1 then factory var (* the var references a parameter of a child lambda of the calling lambda *)
      else factory var (* the var references a parameter of a parent lambda of the calling lambda *) in
    let annotate_box_var = fun factory box_factory depth args_pos_list var ->
      match var with
      | VarParam (_, var_pos) -> annotate_box_var_pos factory box_factory depth args_pos_list var_pos var
      | VarBound (_, depth_offset, var_pos) ->
        let dest_depth = compute_bound_var_depth depth depth_offset in
        annotate_box_var_pos factory box_factory dest_depth args_pos_list var_pos var
      | VarFree _ -> factory var in

    let rec annotate_set = fun depth args_pos_list var value_expr' ->
      let annotated_value_expr' = annotate_boxes_traversal depth args_pos_list value_expr' in
      annotate_box_var
        (fun var -> Set' (var, annotated_value_expr'))
        (fun var -> BoxSet' (var, annotated_value_expr'))
        depth args_pos_list var

    and annotate_boxes_list = fun depth args_pos_list expr'_list ->
      List.map (annotate_boxes_traversal depth args_pos_list) expr'_list

    and annotate_applic = fun factory depth args_pos_list operator_expr' operands_expr'_list ->
      let annotated_operator_expr' = annotate_boxes_traversal depth args_pos_list operator_expr' in
      let annotated_operands_expr' = annotate_boxes_list depth args_pos_list operands_expr'_list in
      factory annotated_operator_expr' annotated_operands_expr'

    and annotate_lambda = fun depth args_pos_list body_expr' ->
      annotate_boxes_traversal (depth + 1) args_pos_list body_expr'

    and annotate_boxes_traversal = fun depth args_pos_list expr' ->
      match expr' with
      | Const' _ -> expr'

      | Var' var ->
        annotate_box_var
          (fun var -> Var' var)
          (fun var -> BoxGet' var)
          depth args_pos_list var

      | If' (test, dit, dif) -> If' (
          annotate_boxes_traversal depth args_pos_list test,
          annotate_boxes_traversal depth args_pos_list dit,
          annotate_boxes_traversal depth args_pos_list dif
        )

      | Seq' expr'_list -> Seq' (annotate_boxes_list depth args_pos_list expr'_list)
      | Set' (var, value_expr') -> annotate_set depth args_pos_list var value_expr'
      | Def' (var, value_expr') -> Def' (var, (annotate_boxes_traversal depth args_pos_list value_expr'))
      | Or' expr'_list -> Or' (annotate_boxes_list depth args_pos_list expr'_list)
      | LambdaSimple' (arg_names, body_expr') -> LambdaSimple' (arg_names, (annotate_lambda depth args_pos_list body_expr'))
      | LambdaOpt' (req_arg_names, opt_arg_name, body_expr') -> LambdaOpt' (req_arg_names, opt_arg_name, (annotate_lambda depth args_pos_list body_expr'))

      | Applic' (operator_expr', operands_expr'_list) ->
        annotate_applic
          (fun annotated_operator_expr' annotated_operands_expr' ->
            Applic' (annotated_operator_expr', annotated_operands_expr'))
          depth
          args_pos_list
          operator_expr'
          operands_expr'_list

      | ApplicTP' (operator_expr', operands_expr'_list) ->
        annotate_applic
          (fun annotated_operator_expr' annotated_operands_expr' ->
            ApplicTP' (annotated_operator_expr', annotated_operands_expr'))
          depth
          args_pos_list
          operator_expr'
          operands_expr'_list

      | Box' _ -> expr'
      | BoxGet' _ -> expr'
      | BoxSet' _ -> expr' in

    let transform_body = fun () ->
      let annotated_body_expr' =
        if annotation_args = [] then body_expr'
        else
          let set_boxes_expr'_list = List.map
            (fun (arg_name, pos) ->
              let var_param = VarParam (arg_name, pos) in
              Set' (var_param, Box' var_param))
            annotation_args in
          let body_expr'_list =
            let args_pos_list = List.map (fun (_, pos) -> pos) annotation_args in
            let annotated_body_expr' = annotate_boxes_traversal 1 args_pos_list body_expr' in
            match annotated_body_expr' with
            | Seq' expr'_list -> expr'_list
            | _ -> [annotated_body_expr'] in
          Seq' (set_boxes_expr'_list @ body_expr'_list) in
      factory annotated_body_expr' in

    transform_body () in

  let annotate_boxes_and_find_var_accesses = fun expr' ->
    let get_var_access_var = fun depth var read_write ->
      match var with
      | VarParam (_, pos) -> [(depth, pos, None, read_write)]
      | VarBound (_, depth_offset, pos) ->
        let dest_depth = compute_bound_var_depth depth depth_offset in
        [(dest_depth, pos, None, read_write)]
      | VarFree _ -> [] in

    let rec do_set = fun depth var value_expr' ->
      let var_var_access = get_var_access_var depth var false in
      let (
        annotated_value_expr',
        value_expr'_var_accesses
      ) = do_traversal depth value_expr' in
      let var_accesses = package_ordered_var_accesses depth [
        value_expr'_var_accesses;
        var_var_access
      ] in
      (Set' (var, annotated_value_expr'), var_accesses)

    and do_def = fun depth var value_expr' ->
      let (
        annotated_value_expr',
        var_acceses
      ) = do_traversal depth value_expr' in
      (Def' (var, annotated_value_expr'), var_acceses)

    and do_if = fun depth test dit dif ->
      let (annotated_test, test_var_accesses) = do_traversal depth test in
      let (annotated_dit, dit_var_accesses) = do_traversal depth dit in
      let (annotated_dif, dif_var_accesses) = do_traversal depth dif in
      let annoated_if = If' (
        annotated_test,
        annotated_dit,
        annotated_dif
      ) in
      let var_accesses = package_ordered_var_accesses depth [
        test_var_accesses;
        dit_var_accesses @ dif_var_accesses
      ] in
      (annoated_if, var_accesses)

    and do_list = fun depth expr'_list ->
      (* expr' list -> (expr', var_access list) list *)
      let annotated_and_var_accesses_list = List.map (do_traversal depth) expr'_list in
      List.split annotated_and_var_accesses_list

    and do_unordered_list = fun depth expr'_list ->
      let (annoated_expr'_list, var_acceses_list_list) = do_list depth expr'_list in
      let all_expr'_var_acceses_list = List.flatten var_acceses_list_list in
      (annoated_expr'_list, all_expr'_var_acceses_list)

    and package_ordered_var_accesses = fun depth var_accesses_list_list ->
      (* TODO actually implemented this when we understand what the requirements are *)
      List.flatten var_accesses_list_list

    and do_ordered_list = fun depth expr'_list ->
      (* TODO actually implemented this when we understand what the requirements are *)
      let (annoated_expr'_list, var_acceses_list_list) = do_list depth expr'_list in
      let ordered_var_accesses_list_list = package_ordered_var_accesses
        depth
        var_acceses_list_list in
      (annoated_expr'_list, ordered_var_accesses_list_list)

    and do_seq = fun depth expr'_list ->
      let (
        annoated_expr'_list,
        ordered_var_accesses_list_list
      ) = do_ordered_list depth expr'_list in
      (Seq' annoated_expr'_list, ordered_var_accesses_list_list)

    and do_or = fun depth expr'_list ->
      let (
        annoated_expr'_list,
        ordered_var_accesses_list_list
      ) = do_ordered_list depth expr'_list in
      (Or' annoated_expr'_list, ordered_var_accesses_list_list)

    and do_applic = fun depth operator_expr' operands_expr'_list ->
      let (
        annotated_operator_expr',
        annotated_operand_expr'_list,
        var_accesses
      ) = do_applic_core depth operator_expr' operands_expr'_list in
      (Applic' (annotated_operator_expr', annotated_operand_expr'_list), var_accesses)
    and do_applicTP = fun depth operator_expr' operands_expr'_list ->
      let (
        annotated_operator_expr',
        annotated_operand_expr'_list,
        var_accesses
      ) = do_applic_core depth operator_expr' operands_expr'_list in
      (ApplicTP' (annotated_operator_expr', annotated_operand_expr'_list), var_accesses)

    and do_applic_core = fun depth operator_expr' operands_expr'_list ->
      let (
        annotated_operator_expr',
        operator_var_accesses
      ) = do_traversal depth operator_expr' in
      let (
        annotated_operand_expr'_list,
        operand_var_accesses
      ) = do_unordered_list depth operands_expr'_list in (
        annotated_operator_expr',
        annotated_operand_expr'_list,
        operator_var_accesses @ operand_var_accesses
      )

    and do_lambda = fun factory depth arg_names body_expr' ->
      (* 1. get all the var accesses in the body
         2. find the var accesses for this lambda
         3. filter out vars according to rules 1 and 2
         4. annotated boxes (i.e. annotate all accesses as box get/set and add a set box at the beginning)
         5. filter out all vars which are parameters of this lambda
         6. map the rest of the accesses to replace the highest lambda to be this lambda
         7. return the var accesses list *)
      let depth = depth + 1 in
      let (
        annotated_body_expr',
        body_var_accesses
      ) = do_traversal depth body_expr' in

      let (var_accesses, parent_lambdas_var_accesses) = List.partition
        (fun (dest_depth, _, _, _) -> dest_depth = depth)
        body_var_accesses in

      let var_accesses = List.filter
        (fun (_, pos, highest_lambda_opt, read_write) ->
          List.exists
            (fun (_, other_pos, other_highest_lambda_opt, other_read_write) ->
              pos = other_pos &&
              read_write <> other_read_write &&
              match highest_lambda_opt, other_highest_lambda_opt with
              | None, None -> false
              | Some highest_lambda, Some other_highest_lambda -> (
                match highest_lambda, other_highest_lambda with
                | LambdaSimple' _, LambdaSimple' _ -> highest_lambda != other_highest_lambda
                | LambdaOpt' _, LambdaOpt' _ -> highest_lambda != other_highest_lambda
                | _ -> true
              )
              | _ -> true)
          var_accesses)
        var_accesses in

      let annotated_body_expr' =
        let args_pos_list = List.map (fun (_, pos, _, _) -> pos) var_accesses in
        let unique_args_pos_list = List.sort_uniq compare args_pos_list in
        let annotation_args = List.map
          (fun pos ->
            let arg_name = List.nth arg_names pos in
            (arg_name, pos))
          unique_args_pos_list in
        box_arguments factory annotation_args annotated_body_expr' in

      let parent_lambdas_var_accesses = List.map
        (fun (dest_depth, pos, _, read_write) ->
          (dest_depth, pos, Some annotated_body_expr', read_write))
        parent_lambdas_var_accesses in

      (annotated_body_expr', parent_lambdas_var_accesses)

    and do_lambda_simple = fun depth arg_names body_expr' ->
      do_lambda
        (fun annotated_body_expr' -> LambdaSimple' (arg_names, annotated_body_expr'))
        depth
        arg_names
        body_expr'

    and do_lambda_opt = fun depth req_arg_names opt_arg_name body_expr' ->
      do_lambda
        (fun annotated_body_expr' -> LambdaOpt' (req_arg_names, opt_arg_name, annotated_body_expr'))
        depth
        (req_arg_names @ [opt_arg_name])
        body_expr'

    (* data structure for the return value of the recursio:
      (annotated_expr', [(depth, pos, highest_lambda option, read/write)])
      read - true
      write - false
    *)
    and do_traversal = fun depth expr' ->
      match expr' with
      | Const' _ -> (expr', [])
      | Var' var -> (expr', get_var_access_var depth var true)
      | If' (test, dit, dif) -> do_if depth test dit dif
      | Seq' expr'_list -> do_seq depth expr'_list
      | Set' (var, value_expr') -> do_set depth var value_expr'
      | Def' (var, value_expr') -> do_def depth var value_expr'
      | Or' expr'_list -> do_or depth expr'_list
      | Applic' (operator_expr', operands_expr'_list) -> do_applic depth operator_expr' operands_expr'_list
      | ApplicTP' (operator_expr', operands_expr'_list) -> do_applicTP depth operator_expr' operands_expr'_list

      (* returns with possibly annotated body *)
      | LambdaSimple' (arg_names, body_expr') -> do_lambda_simple depth arg_names body_expr'
      | LambdaOpt' (req_arg_names, opt_arg_name, body_expr') -> do_lambda_opt depth req_arg_names opt_arg_name body_expr'

      (* impossible case, were annotate right now,
         so if we got a box, it's probably a bug *)
      | Box' _ -> raise X_syntax_error
      | BoxGet' _ -> raise X_syntax_error
      | BoxSet' _ -> raise X_syntax_error in

    do_traversal 0 expr' in

  let (annotated_expr', _) = annotate_boxes_and_find_var_accesses e in
  annotated_expr'

let annotate_lexical_addresses e = annotate_lexical_addresses_helper e;;

let annotate_tail_calls e = annotate_tail_calls_helper e;;

let box_set e = annotate_boxes_helper e;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;

end;; (* struct Semantics *)