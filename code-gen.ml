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
      Const'(Sexpr(Nil));
      Const'(Void);
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
    List.assoc fvar_name fvars_tbl;;

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

(*
============== Code generation ==============
*)

  let word_size = 8;;
  let word_size_str = "8";;

  let rax_reg_str =  "rax";;
  let rbx_reg_str =  "rbx";;
  let rbp_reg_str =  "rbp";;

  let const_table_label = "const_tbl";;
  let fvar_tabel_label = "fvar_tbl";;

  let mov_to_register = fun reg from ->
    "mov " ^ reg ^ ", " ^ from;;

  let mov_from_register = fun mov_to reg ->
    "mov " ^ mov_to ^ ", " ^ reg;;

  (*========== To register qword indirect ==========*)

  let mov_to_register_qword_indirect = fun reg from ->
    Printf.sprintf "mov %s, qword [%s]" reg from;;

  let mov_to_register_from_indirect = fun reg reg_indirect offset ->
    let offset_from_reg =
      Printf.sprintf "%s + %d*%d" reg_indirect word_size offset in
    mov_to_register_qword_indirect reg offset_from_reg;;

  let mov_to_register_from_rbp = fun reg offset ->
    mov_to_register_from_indirect reg rbp_reg_str offset;;

  let mov_to_register_var_param = fun reg minor ->
    mov_to_register_from_rbp reg (4 +  minor);;

  let get_lex_env_code = fun reg ->
    mov_to_register_from_rbp reg 2;;

  (*========== To register qword indirect ==========*)
  let mov_from_register_to_qword_indirect = fun to_address reg ->
    Printf.sprintf "mov qword [%s], %s" to_address reg;;

  let mov_from_register_to_indirect = fun mov_to offset from_reg ->
    let offset_from_reg =
      Printf.sprintf "%s + %d*%d" mov_to word_size offset in
    mov_from_register_to_qword_indirect offset_from_reg from_reg;;

  let mov_from_register_to_rbp_indirect = fun offset from ->
    mov_from_register_to_indirect rbp_reg_str offset from;;

  let mov_to_register_var_param_set = fun minor from ->
    mov_from_register_to_rbp_indirect (4 +  minor) from;;

  (*========== General code ==========*)
  let ret_void_code = mov_to_register rax_reg_str "SOB_VOID_ADDRESS";;

  let concat_list_of_code = fun list ->
    String.concat
    "\n"
    list;;

  let comment_indexer = ref(0);;
  let comment_index () =
    comment_indexer := !comment_indexer + 1;
    Printf.sprintf "%d" !comment_indexer;;

  let if_else_indexer = ref(0);;
  let exit_indexer = ref(0);;
  let else_label_of_if () =
    if_else_indexer := !if_else_indexer + 1;
    Printf.sprintf "Lelse%d" !if_else_indexer;;
  let exit_label () =
    exit_indexer := !exit_indexer + 1;
    Printf.sprintf "Lexit%d" !exit_indexer;;

  let lambda_code_indexer = ref(0);;
  let lcode_label () =
    lambda_code_indexer := !lambda_code_indexer + 1;
    Printf.sprintf "Lcode%d" !lambda_code_indexer;;

  let lambda_code_cont_indexer = ref(0);;
  let lcont_label () =
    lambda_code_cont_indexer := !lambda_code_cont_indexer + 1;
    Printf.sprintf "Lcont%d" !lambda_code_cont_indexer;;

  let env_depth_ref = ref(-1);;
  let inc_env_depth () =  env_depth_ref := !env_depth_ref + 1;;
  let dec_env_depth () =  env_depth_ref := !env_depth_ref - 1;; 

  let enclosing_labmda_param_vars_ref = ref(0);;
  let set_enclosing_labmda_param_vars_ref = fun value -> 
    enclosing_labmda_param_vars_ref := value;;

  let var_to_string = fun var ->
    match var with
    | VarFree(var_name) -> var_name
    | VarParam(var_name, minor) -> Printf.sprintf "name: %s , minor %d" var_name minor
    | VarBound (var_name, major, minor) ->
       Printf.sprintf "name: %s , major %d, minor %d" var_name major minor;;

  let generate_code_wrapper = fun consts_tbl fvars expr' ->

    (*========== Const ==========*)

    let generate_code_for_constant = fun const ->
      let const_index = get_index_of_const_in_const_tbl const consts_tbl in
      let const_code_address = const_table_label ^ " + " ^ (string_of_int const_index) in
        "mov rax, " ^ const_code_address in

    (*========== Vars get ==========*)

    let get_var_offset_code_from_fvars_tbl = fun var_name fvars ->
      let var_offset = get_index_in_fvars_tbl var_name fvars in
        fvar_tabel_label ^ " + " ^ (string_of_int var_offset) in

    let generate_code_for_free_var = fun var_name ->
      let comment = Printf.sprintf ";Get VarFree(%s)" var_name in
      let code_adress = get_var_offset_code_from_fvars_tbl var_name fvars in
      concat_list_of_code 
      [
        comment; 
        "mov rax, qword [" ^ code_adress ^ "]"
      ] in

    let generate_code_for_var_param = fun minor ->
      let comment = Printf.sprintf ";Get VarParam(%d)" minor in
      concat_list_of_code 
      [
        comment; 
        "mov rax, PVAR(" ^ (string_of_int minor) ^ ")"
      ] in

    let generate_code_for_var_bound = fun major minor ->
      let comment = Printf.sprintf ";Get VarBound(%d, %d)" major minor in
      concat_list_of_code
      [
        comment;
        "mov rax, ENV";
        "mov rax, [rax + WORD_SIZE * " ^ (string_of_int major) ^ "]";
        "mov rax, [rax + WORD_SIZE * " ^ (string_of_int minor) ^ "]"
      ] in

    let generate_code_for_var = fun var ->
      match var with
      | VarFree(var_name) -> generate_code_for_free_var var_name
      | VarParam(_, minor) -> generate_code_for_var_param minor
      | VarBound (_, major, minor) -> generate_code_for_var_bound major minor in

    (*========== Vars set ==========*)

    let generate_code_for_free_var_set = fun var_name ->
      let comment = Printf.sprintf ";Set VarFree(%s)" var_name in
      let var_adress = get_var_offset_code_from_fvars_tbl var_name fvars in
      let set_code = mov_from_register_to_qword_indirect var_adress rax_reg_str in
        concat_list_of_code [comment; set_code; ret_void_code] in

    let generate_code_for_var_param_set = fun minor ->
      let comment = Printf.sprintf ";Set VarParam(%d)" minor in
      let var_code = mov_to_register_var_param_set minor rax_reg_str in
        concat_list_of_code [comment; var_code] in

    let generate_code_for_var_bound_set = fun major minor ->
      let comment = Printf.sprintf ";Set VarBound(%d, %d)" major minor in
      let indirect_from_rbx = mov_to_register_from_indirect rbx_reg_str rbx_reg_str in
        concat_list_of_code
          [
            comment;
            get_lex_env_code rbx_reg_str;
            indirect_from_rbx major;
            mov_from_register_to_indirect rbx_reg_str minor rax_reg_str;
            ret_void_code;
          ] in

    let generate_code_for_set_var = fun var ->
      match var with
      | VarFree(var_name) -> generate_code_for_free_var_set var_name
      | VarParam(_, minor) -> generate_code_for_var_param_set minor
      | VarBound (_, major, minor) -> generate_code_for_var_bound_set major minor in

    let rec generate_code_for_set = fun var expr' ->
      let generated_code_for_expr' = generate_code expr' in
      let generated_code_for_var' = generate_code_for_set_var var in
      concat_list_of_code [generated_code_for_expr'; generated_code_for_var']

    (*========== Sequence ==========*)

    and generate_code_for_sequence = fun expr'_list ->
      let inner_comment_index = comment_index () in
      let open_comment = "; Open sequence " ^ inner_comment_index in
      let close_comment = "; Close sequence " ^ inner_comment_index in
      let code_list = List.map generate_code expr'_list in
        concat_list_of_code ((open_comment :: code_list) @ [close_comment])

    (*========== If ==========*)

    and generate_code_for_if = fun test dit dif ->
      let inner_comment_index = comment_index () in
      let test_comment = "; test " ^ inner_comment_index in
      let dit_comment = "; dit " ^ inner_comment_index in
      let tif_comment = "; dif " ^ inner_comment_index in
      let test_code = generate_code test in
      let cmp_test = Printf.sprintf "cmp %s, SOB_FALSE_ADDRESS" rax_reg_str in
      let else_label_name = else_label_of_if () in
      let exit_label_name = exit_label () in
      let jmp_if_false = Printf.sprintf "je %s" else_label_name in
      let dit_code = generate_code dit in
      let jmp_exit = Printf.sprintf "jmp %s" exit_label_name in
      let dif_code = generate_code dif in
      concat_list_of_code
      [
        test_comment;
        test_code;
        cmp_test;
        jmp_if_false;
        dit_comment;
        dit_code;
        jmp_exit;
        (else_label_name ^ ":");
        tif_comment;
        dif_code;
        (exit_label_name ^ ":")
      ]

    (*========== Or ==========*)

      and generate_code_for_or = fun expr'_list ->
        let inner_comment_index = comment_index () in
        let comment = "; Or " ^ inner_comment_index in
        let cmp_test = Printf.sprintf "cmp %s, SOB_FALSE_ADDRESS" rax_reg_str in
        let exit_label_name = exit_label () in
        let jmp_true_to_exit = Printf.sprintf "jne %s" exit_label_name in
        let cmp_and_jmp_code = concat_list_of_code [cmp_test; jmp_true_to_exit] in
        let cmp_and_jmp_code = cmp_and_jmp_code ^ "\n" in
        let code_list =
          List.fold_right
          (fun expr' acc ->
            let generated_expr'_code = generate_code expr' in
              [cmp_and_jmp_code; generated_expr'_code] @ acc
          )
          expr'_list
          []
          in
        let code_list =
          match code_list with
          | [] -> [mov_from_register rax_reg_str "SOB_FALSE_ADDRESS"]
          | _ :: rest -> rest in
        let code_list = code_list @ [exit_label_name ^ ":"] in
        concat_list_of_code (comment :: code_list)

    (*========== Box get set ==========*)

    and generate_code_for_box_get = fun var ->
      let inner_comment_index = comment_index () in
      let var_string = var_to_string var in
      let comment =
        Printf.sprintf "; BoxGet%s of %s" inner_comment_index var_string in
      let generated_code = generate_code_for_var var in
      let mov_value_code = mov_to_register_qword_indirect rax_reg_str rax_reg_str in
      concat_list_of_code
      [
        comment;
        generated_code;
        mov_value_code
      ]

    and generate_code_for_box_set = fun var expr' ->
      let inner_comment_index = comment_index () in
      let var_string = var_to_string var in
      let comment =
        Printf.sprintf "; BoxSet%s of %s" inner_comment_index var_string in
      let expr_generated_code = generate_code expr' in
      let push_rax_code = Printf.sprintf "push %s" rax_reg_str in
      let code_get_pointer = generate_code_for_var var in
      let pop_to_rax_indirect_code = Printf.sprintf "pop qword [%s]" rax_reg_str in
      concat_list_of_code
      [
        comment;
        expr_generated_code;
        push_rax_code;
        code_get_pointer;
        pop_to_rax_indirect_code;
        ret_void_code
      ]

    and generate_code_for_box = fun var ->
      let inner_comment_index = comment_index () in
      let var_string = var_to_string var in
      let comment =  Printf.sprintf "; Box%s of %s" inner_comment_index var_string in
      let make_var_code = generate_code_for_var var in
      let mov_var_ptr_to_rbx = mov_to_register rbx_reg_str rax_reg_str in
      let alloc_box_code = Printf.sprintf "MALLOC %s WORD_SIZE" rax_reg_str in
      let set_var_ptr = mov_from_register_to_qword_indirect rax_reg_str rbx_reg_str in
      concat_list_of_code
      [
        comment;
        make_var_code;
        mov_var_ptr_to_rbx;
        alloc_box_code;
        set_var_ptr
      ]

    (*========== Define ==========*)

    and generate_code_for_def = fun var expr' ->
      let comment = "; Define" in
      let generated_code = generate_code_for_set var expr' in
      concat_list_of_code [comment; generated_code]

    (*========== Lambda ==========*)

    and generate_code_for_lambda = fun lambda -> 
      inc_env_depth ();
      let generated_code = 
        match lambda with
        | LambdaSimple'(arg_names, body_expr') -> 
            generate_code_for_lambda_simple body_expr' (List.length arg_names)
        | LambdaOpt'(req_arg_names, opt_arg_name, body_expr') -> raise X_not_yet_implemented 
        | _ -> raise X_syntax_error in
      dec_env_depth ();
      generated_code

    and generate_code_for_lambda_simple = fun body_expr' number_of_args ->
      let env_depth = !env_depth_ref in
      let current_enclosing_labmda_param_vars = string_of_int !enclosing_labmda_param_vars_ref in
      set_enclosing_labmda_param_vars_ref number_of_args;
      let lcode_label_name = lcode_label () in
      let lcont_label_name = lcont_label () in
      let code = 
      [
        "; lambda simple depth " ^ (string_of_int env_depth);
        "MALLOC rcx, WORD_SIZE*" ^ (string_of_int (env_depth+1));
        "mov rbx, ENV";
        "COPY_ARRAY_STATIC rbx, 0, rcx, 1, " ^ (string_of_int (env_depth));
        "MALLOC rbx, WORD_SIZE*" ^ current_enclosing_labmda_param_vars;
        "mov qword [rcx], rbx";
        "PVAR_ADDR(rax, 0)";
        "COPY_ARRAY_STATIC rax, 0, rbx, 0, " ^ current_enclosing_labmda_param_vars;
        "MAKE_CLOSURE(rax, rcx, " ^ lcode_label_name ^ ")";
        "jmp " ^ lcont_label_name;
        lcode_label_name ^ ":";
        "push rbp";
        "mov rbp, rsp";
        generate_code body_expr';
        "leave";
        "ret";
        lcont_label_name ^ ":"
      ] in
      concat_list_of_code code

    and generate_code_for_applic = fun operator_expr' operands_expr'_list ->
      let push_args = 
        List.fold_left
        (fun acc expr' -> generate_code expr' :: "push rax" :: acc )
        []
        operands_expr'_list in
      let rest_of_the_code = 
      [
        "push " ^ (string_of_int (List.length operands_expr'_list));
        generate_code operator_expr';
        "push qword [rax + TYPE_SIZE]";
        "call [rax + TYPE_SIZE + WORD_SIZE]";
        "add rsp, WORD_SIZE  ; pop env";
        "pop rbx  ; pop args count";
        "shl rbx, 3  ; rbx = rbx * 8";
        "add rsp, rbx  ; pop args"
      ] in
      concat_list_of_code ([";Applic"] @ push_args @ rest_of_the_code)

    (*========== Generate code ==========*)

    and generate_code = fun expr' ->
      match expr' with
      | Const'(const) -> generate_code_for_constant const

      | Var'(var) -> generate_code_for_var var
      | Box'(var) -> generate_code_for_box var
      | BoxGet'(var) -> generate_code_for_box_get var
      | BoxSet'(var, expr') -> generate_code_for_box_set var expr'

      | If'(test, dit, dif) -> generate_code_for_if test dit dif

      | Seq'(expr'_list) -> generate_code_for_sequence expr'_list

      | Set'(var, expr') -> generate_code_for_set var expr'
      | Def'(var, expr') -> generate_code_for_def var expr'

      | Or'(expr'_list) -> generate_code_for_or expr'_list

      | LambdaSimple'(arg_names, body_expr') -> generate_code_for_lambda expr'
      | LambdaOpt'(req_arg_names, opt_arg_name, body_expr') -> generate_code_for_lambda expr'

      | Applic'(operator_expr', operands_expr'_list) -> generate_code_for_applic operator_expr' operands_expr'_list
      | ApplicTP'(operator_expr', operands_expr'_list) -> raise X_not_yet_implemented in
    generate_code expr';;

  let make_consts_tbl = fun asts ->
    build_const_tbl asts

  let make_fvars_tbl asts =
    build_fvars_tbl asts;;

  let generate consts fvars e =
    generate_code_wrapper consts fvars e;;
end;;

