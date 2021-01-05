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

  let remove_dup_from_list = fun list eq_function ->
    List.fold_left
    (fun acc item ->
      let is_dup = List.exists (fun param_item -> eq_function param_item item) acc in
      if is_dup then acc else acc @ [item]
    )
    []
    list;;

  let remove_dup_consts_from_const_list = fun sexpr_list ->
    remove_dup_from_list sexpr_list constant_eq;;

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

  (* Step 3  extend the const list *)
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
    List.fold_right
    (fun const acc -> 
      let extended_const = extend_const const in
        extended_const @ acc 
    )
    const_list
    []

  (* Make asm code for literals *)
  let const_tbl_label_plus = "const_tbl+";;

  let make_literal_void_asm_code = "db T_VOID";;
  let make_literal_nil_asm_code = "db T_NIL";;
  let make_literal_bool_asm_code = fun bool ->
    let bool_val = if bool then 1 else 0 in
      Printf.sprintf "db T_BOOL, %d" bool_val;;
  let make_literal_char_asm_code = fun ch ->
    Printf.sprintf "db T_CHAR, \"%s\"" (String.make 1 ch);;
  let make_literal_rational_asm_code = fun num den ->
    Printf.sprintf "MAKE_LITERAL_RATIONAL(%d, %d)" num den;;
  let make_literal_float_asm_code = fun float ->
    Printf.sprintf "MAKE_LITERAL_FLOAT(%f)" float;;
  let make_literal_string_asm_code = fun str ->
    let str_len = String.length str in
      Printf.sprintf "MAKE_LITERAL_STRING(%d, \"%s\")" str_len str;;
  let make_literal_symbol_asm_code = fun addr ->
      Printf.sprintf "MAKE_LITERAL_SYMBOL(%s%d)" const_tbl_label_plus addr;;
  let make_literal_pair_asm_code = fun addr_car addr_cdr ->
      Printf.sprintf "MAKE_LITERAL_PAIR(%s%d, %s%d)" 
        const_tbl_label_plus addr_car 
        const_tbl_label_plus addr_cdr;;


  let get_sob_macro_size_of_sexpr_type = fun sexpr ->
    match sexpr with
    | Bool(_) -> "SOB_BOOL_SIZE"
    | Nil -> "SOB_NIL_SIZE"
    | Number(number) -> (
        match number with
        | Fraction(_) -> "SOB_RATIONAL_SIZE"
        | Float(_) -> "SOB_FLOAT_SIZE"
      )
    | Char(_) -> "SOB_CHAR_SIZE"
    | String(string) -> (
        let str_length = String.length string in
          Printf.sprintf "SOB_STRING_SIZE(%d)" str_length
    )
    | Symbol(_) -> "SOB_SYMBOL_SIZE"
    | Pair(_) -> "SOB_PAIR_SIZE";;

  let get_sob_macro_size_of_const = fun const ->
    match const with
    | Void -> "SOB_VOID_SIZE"
    | Sexpr(sexpr) -> get_sob_macro_size_of_sexpr_type sexpr;;


  (* sob sizes *)
  let tag_sob_size = 1;;
  let nil_sob_size = tag_sob_size;;
  let void_sob_size = tag_sob_size;;

  let bool_sob_size = tag_sob_size + 1;;
  let char_sob_size = tag_sob_size + 1;; 

  let word_sob_size = tag_sob_size + 8;;
  let float_sob_size = word_sob_size;;
  let symbol_sob_size = word_sob_size;;

  let string_sob_size = fun string ->
    let str_length = String.length string in
      word_sob_size + str_length;;

  let double_word_sob_size = tag_sob_size + 16;;
  let rational_word_sob_size = double_word_sob_size;;
  let pair_sob_size = double_word_sob_size;;

  let get_sob_size_of_sexpr_type = fun sexpr ->
  match sexpr with
  | Bool(_) -> bool_sob_size
  | Nil -> nil_sob_size
  | Number(number) -> (
      match number with
      | Fraction(_) -> rational_word_sob_size
      | Float(_) -> float_sob_size
    )
  | Char(_) -> char_sob_size
  | String(string) -> string_sob_size string
  | Symbol(_) -> symbol_sob_size
  | Pair(_) -> pair_sob_size;;

  let get_sob_size_of_const = fun const ->
    match const with
    | Void -> void_sob_size
    | Sexpr(sexpr) -> get_sob_size_of_sexpr_type sexpr

  (* step 5 *)

  let get_const_from_tuple = fun const_tuple ->
    fst const_tuple;;

  let get_index_from_const_tuple = fun const_tuple ->
    fst (snd const_tuple);;

  let get_code_from_tuple = fun const_tuple ->
    snd (snd const_tuple);;

  let build_consts_tbl_from_const_list = fun const_list ->
    let rec build_consts_tbl_rec = fun const_list tuple_const_list index ->
      match const_list with
      | [] -> tuple_const_list
      | const :: rest -> (
        let const_asm_code = make_asm_code const tuple_const_list index in
        let const_sob_size =  get_sob_size_of_const const in
        let const_index = index + const_sob_size in
        let const_tuple = (const, (index, const_asm_code)) in
         build_consts_tbl_rec rest (tuple_const_list @ [const_tuple]) const_index
      )

    and get_tuple_of_const_from_tbl = fun const const_tbl ->
      let eq = constant_eq const in
        List.find 
        (fun tuple -> 
          let c = get_const_from_tuple tuple in
            eq c
        )
        const_tbl

    and make_asm_code_number_from_const_tbl = fun number index ->
      match number with
      | Fraction(num, den) -> make_literal_rational_asm_code num den
      | Float(float) -> make_literal_float_asm_code float

    and make_asm_code_symbol_from_const_tbl = fun symbol_string current_tuple_const_tbl ->
      let string_const = Sexpr(String(symbol_string)) in
      let sting_tuple = get_tuple_of_const_from_tbl string_const current_tuple_const_tbl in
      let string_index = get_index_from_const_tuple sting_tuple in
        make_literal_symbol_asm_code string_index

    and make_asm_code_pair_from_const_tbl = fun car cdr current_tuple_const_tbl ->
      let car_tuple = get_tuple_of_const_from_tbl (Sexpr(car)) current_tuple_const_tbl in
      let cdr_tuple = get_tuple_of_const_from_tbl (Sexpr(cdr)) current_tuple_const_tbl in
      let car_index = get_index_from_const_tuple car_tuple in
      let cdr_index = get_index_from_const_tuple cdr_tuple in
        make_literal_pair_asm_code car_index cdr_index

    and make_asm_code =  fun const current_tuple_const_tbl index ->
      match const with
      | Void -> make_literal_void_asm_code
      | Sexpr(sexpr) -> (
        match sexpr with
        | Nil -> make_literal_nil_asm_code
        | Bool(bool) -> make_literal_bool_asm_code bool
        | Number(number) -> 
            make_asm_code_number_from_const_tbl number index
        | Char(ch) -> make_literal_char_asm_code ch
        | String(string) -> 
            make_literal_string_asm_code string
        | Symbol(string_symbol) -> 
            make_asm_code_symbol_from_const_tbl string_symbol current_tuple_const_tbl
        | Pair(car, cdr) -> 
            make_asm_code_pair_from_const_tbl car cdr current_tuple_const_tbl
      ) in
    build_consts_tbl_rec const_list [] 0;;

  let init_const_list_for_const_tbl = 
    [
      Const'(Void);
      Const'(Sexpr(Nil));
      Const'(Sexpr(Bool(false)));
      Const'(Sexpr(Bool(true)));
    ];;

  let build_const_tbl = fun expr'_list ->
    let expr'_list = (init_const_list_for_const_tbl @ expr'_list) in 
    let extracted_consts_list = extract_const_from_expr'_list expr'_list in
    let extracted_consts_list = 
      remove_dup_consts_from_const_list extracted_consts_list in
    let extended_consts_list = extend_const_list (extracted_consts_list) in
    let extracted_consts_list = 
      remove_dup_consts_from_const_list extended_consts_list in
    build_consts_tbl_from_const_list extracted_consts_list;;

  let get_index_of_const_in_const_tbl = fun const const_tbl ->
    let const_tuple = List.assoc const const_tbl in
      fst const_tuple;;

  let generate_asm_code_from_const_tbl = fun const_tbl ->
    List.fold_left
    (fun acc tuple ->
      let asm_tuple_code = get_code_from_tuple tuple in
      let asm_tuple_code = asm_tuple_code ^ "\n" in
        acc ^ asm_tuple_code
    )
    ""
    const_tbl;;

(* 
============== Free var table ==============
*)

  let extract_fvar_string = fun var ->
  match var with
  | VarFree var -> [var]
  | _ -> []

  let rec extract_fvar_names_from_expr'_list = fun expr'_list ->
    List.fold_left
    (fun acc expr' ->
      let extracted_fvars = extract_fvar_names_from_expr' expr' in 
        acc @ extracted_fvars
    )
    []
    expr'_list

  and extract_fvar_names_from_var_and_expr'_list = fun var expr'_list ->
    let fvar = extract_fvar_string var in
    let extracted_fvars_from_list = extract_fvar_names_from_expr'_list expr'_list in
      fvar @ extracted_fvars_from_list

  and extract_fvar_names_from_expr' = fun expr'  ->
    match expr' with
    | Const'(const) -> []
    | Var'(var) -> extract_fvar_string var 
    | Box'(var) -> extract_fvar_string var 
    | BoxGet'(var) -> extract_fvar_string var 
    | BoxSet'(var, expr') -> 
        extract_fvar_names_from_var_and_expr'_list var [expr'] 

    | If'(test, dit, dif) ->
        extract_fvar_names_from_expr'_list [test; dit; dif] 

    | Seq'(expr'_list) ->
        extract_fvar_names_from_expr'_list expr'_list 

    | Set'(var, expr') -> 
        extract_fvar_names_from_var_and_expr'_list var [expr'] 
    | Def'(var, expr') -> 
        extract_fvar_names_from_var_and_expr'_list var [expr'] 

    | Or'(expr'_list) ->
        extract_fvar_names_from_expr'_list expr'_list 

    | LambdaSimple'(arg_names, body_expr') ->
        extract_fvar_names_from_expr' body_expr' 
    | LambdaOpt'(req_arg_names, opt_arg_name, body_expr') ->
        extract_fvar_names_from_expr' body_expr'

    | Applic'(operator_expr', operands_expr'_list) ->
        extract_fvar_names_from_expr'_list (operator_expr' :: operands_expr'_list)
    | ApplicTP'(operator_expr', operands_expr'_list) ->
        extract_fvar_names_from_expr'_list (operator_expr' :: operands_expr'_list);;

  let reserved_fvar_list = [
    "+"; "-"; "*"; "/"; "<"; "="; ">";
    "append"; "apply"; "boolean?"; "car"; "cdr";
    "char->integer"; "char?"; "cons"; "cons*"; "denominator";
    "eq?"; "equal?"; "exact->inexact"; "flonum?"; "fold-left";
    "fold-right"; "gcd"; "integer?"; "integer->char"; "length";
    "list"; "list?"; "make-string"; "map"; "not"; "null?"; "number?";
    "numerator"; "pair?"; "procedure?"; "rational?"; "set-car!";
    "set-cdr!"; "string->list"; "string-length"; "string-ref"; 
    "string-set!"; "string?"; "symbol?"; "zero?"; "symbol->string"
  ];;

  let get_index_in_fvars_tbl = fun fvar_name fvars_tbl ->
    let fvar_tuple = List.find (fun str -> str = fvar_name) fvars_tbl in
      snd fvar_tuple;;

  let make_index_tuple_from_list = fun list ->
    List.mapi
    (fun index item -> (item, (index * 8)))
    list

  let build_fvars_tbl = fun expr'_list ->
    let extracted_fvars_names = extract_fvar_names_from_expr'_list expr'_list in
    let fvars_names = reserved_fvar_list @ extracted_fvars_names in
    let fvars_names = 
      remove_dup_from_list 
      fvars_names
      (fun str1 str2 -> str1 = str2) in
    make_index_tuple_from_list fvars_names

  let make_consts_tbl = fun asts ->
    build_const_tbl asts 
  
  let make_fvars_tbl asts = 
    build_fvars_tbl asts;;

(* 
============== Code generation ==============
*)

  let const_table_label = "const_tbl";;

  let mov_to_register = fun reg from ->
    "mov " ^ reg ^ "," ^ from;;

  let generate_code_wrapper = fun consts_tbl fvars expr' ->
    let generate_code_for_constant = fun const ->
      let const_index = get_index_of_const_in_const_tbl const consts_tbl in
      let const_code_address = const_table_label ^ "+" ^ (string_of_int const_index) in
        mov_to_register "rax" const_code_address in

    let generate_code = fun expr' ->
      match expr' with
      | Const'(const) -> generate_code_for_constant const
      | Var'(var) -> raise X_not_yet_implemented
      | Box'(var) -> raise X_not_yet_implemented 
      | BoxGet'(var) -> raise X_not_yet_implemented
      | BoxSet'(var, expr') -> 
          raise X_not_yet_implemented

      | If'(test, dit, dif) ->
          raise X_not_yet_implemented

      | Seq'(expr'_list) ->
         raise X_not_yet_implemented

      | Set'(var, expr') -> 
          raise X_not_yet_implemented
      | Def'(var, expr') -> 
          raise X_not_yet_implemented

      | Or'(expr'_list) ->
          raise X_not_yet_implemented

      | LambdaSimple'(arg_names, body_expr') ->
          raise X_not_yet_implemented
      | LambdaOpt'(req_arg_names, opt_arg_name, body_expr') ->
          raise X_not_yet_implemented

      | Applic'(operator_expr', operands_expr'_list) ->
          raise X_not_yet_implemented
      | ApplicTP'(operator_expr', operands_expr'_list) ->
          raise X_not_yet_implemented in
    generate_code expr';;

  let generate consts fvars e = 
    generate_code_wrapper consts fvars e;;
end;;
