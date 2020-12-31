#use "semantic-analyser.ml";;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen (* : CODE_GEN *) = struct

(* 
============== Const table ==============
*)

  (* Step 2 and 4 remove duplicates *)
  let constant_eq = fun const1 const2 ->
    match const1, const2 with
    | Void, Void -> true
    | Sexpr(sexpr1), Sexpr(sexpr2) -> sexpr_eq sexpr1 sexpr2
    | _ -> false

  let rec remove_dup_consts_from_const_list = fun sexpr_list ->
    remove_dup_consts sexpr_list []

  and remove_dup_consts = fun sexpr_list clean_list ->
    match sexpr_list with
    | [] -> clean_list
    | hd :: rest -> (
      let is_dup = List.exists (fun sexpr -> constant_eq sexpr hd) clean_list in
      if is_dup
      then remove_dup_consts rest clean_list
      else remove_dup_consts rest (clean_list @ [hd])
    )

  (* Step 1 extract all Const from the asts *)
  let rec extract_const_from_expr'_list = fun expr'_list ->
    List.fold_left 
    (fun acc expr' ->
      let extracted_const_list = extract_all_const expr' in
        acc @ extracted_const_list
    )
    []
    expr'_list

  and extract_const_from_applic = fun operator_expr' operands_expr'_list ->
    extract_const_from_expr'_list (operator_expr' :: operands_expr'_list)

  and extract_all_const = fun expr' ->
  match expr' with
  | Const'(const) -> [const]
  | Var'(var) -> []
  | Box'(var) -> []
  | BoxGet'(var) -> []
  | BoxSet'(var, e') -> extract_all_const e'

  | If'(test, dit, dif) ->
      extract_const_from_expr'_list [test; dit; dif]

  | Seq'(expr'_list) ->
      extract_const_from_expr'_list expr'_list

  | Set'(var, expr') -> extract_all_const expr'
  | Def'(var, expr') -> extract_all_const expr'

  | Or'(expr'_list) ->
      extract_const_from_expr'_list expr'_list

  | LambdaSimple'(arg_names, body_expr') ->
      extract_all_const body_expr'
  | LambdaOpt'(req_arg_names, opt_arg_name, body_expr') ->
      extract_all_const body_expr'

  | Applic'(operator_expr', operands_expr'_list) ->
      extract_const_from_applic operator_expr' operands_expr'_list
  | ApplicTP'(operator_expr', operands_expr'_list) ->
      extract_const_from_applic operator_expr' operands_expr'_list;;

  (* Step 3 and 4 extend the const list and remove duplicates *)
  let rec extend_const = fun const ->
    match const with
    | Void -> [Void]
    | Sexpr sexpr -> extend_sexpr sexpr

  and extend_sexpr = fun sexpr ->
    match sexpr with
    | Symbol(string) -> [Sexpr(String(string)); Sexpr(sexpr)]
    | Pair(car_sexpr, cdr_sexpr) ->
        extend_pair car_sexpr cdr_sexpr
    | _ -> [Sexpr(sexpr)]
  
  and extend_pair = fun car_sepxr cdr_sexpr ->
    let extended_car = extend_sexpr car_sepxr in
    let extended_cdr = extend_sexpr cdr_sexpr in
      extended_car @ extended_cdr @ [Sexpr(Pair(car_sepxr, cdr_sexpr))]

  let extend_const_list = fun const_list ->
    let extended_const_list = 
      List.fold_right
      (fun const acc -> 
        let extended_const = extend_const const in
          extended_const @ acc 
      )
      const_list
      [] in
      remove_dup_consts_from_const_list extended_const_list


  let make_consts_tbl asts = raise X_not_yet_implemented;;
  let make_fvars_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;
end;;

