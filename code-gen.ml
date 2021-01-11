#use "semantic-analyser.ml";;
#use "exp_to_string.ml";;
#use "config.ml";;
open ExpString;;
open CompConfig;;

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

module Code_Gen : CODE_GEN = struct

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
    Printf.sprintf "db T_CHAR, %d" (Char.code ch);;
  let make_literal_rational_asm_code = fun num den ->
    Printf.sprintf "MAKE_LITERAL_RATIONAL(%d, %d)" num den;;
  let make_literal_float_asm_code = fun float ->
    Printf.sprintf "MAKE_LITERAL_FLOAT(%f)" float;;
  let make_literal_string_asm_code = fun str ->
    let str_len = String.length str in
    let chars_codes_str_cs =
      let string_chars_list = string_to_list str in
      let chars_codes = List.map Char.code string_chars_list in
      let chars_codes_strs = List.map (fun char_code -> Printf.sprintf "%d" char_code) chars_codes in
      String.concat ", " chars_codes_strs in
    Printf.sprintf "MAKE_LITERAL_STRING(%d, {%s})" str_len chars_codes_str_cs;;
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
    let rec build_consts_tbl_rec = fun const_list tuple_const_list offset ->
      match const_list with
      | [] -> tuple_const_list
      | const :: rest -> (
        let next_start_offest = offset + (get_sob_size_of_const const) in
        let const_asm_code =
          let code = make_asm_code const tuple_const_list in
          if debug then
            let comment = Printf.sprintf "; offset %d, %s" offset (constant_to_string const) in
            Printf.sprintf "%s %s" code comment
          else code in
        let const_tuple = (const, (offset, const_asm_code)) in
         build_consts_tbl_rec rest (tuple_const_list @ [const_tuple]) next_start_offest
      )

    and get_tuple_of_const_from_tbl = fun const const_tbl ->
      let eq = constant_eq const in
        List.find
        (fun tuple ->
          let c = get_const_from_tuple tuple in
            eq c
        )
        const_tbl

    and make_asm_code_number_from_const_tbl = fun number ->
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

    and make_asm_code =  fun const current_tuple_const_tbl ->
      match const with
      | Void -> make_literal_void_asm_code
      | Sexpr(sexpr) -> (
        match sexpr with
        | Nil -> make_literal_nil_asm_code
        | Bool(bool) -> make_literal_bool_asm_code bool
        | Number(number) ->
            make_asm_code_number_from_const_tbl number
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

  let reserved_fvar_list =
    let reserved_fvar_list_core = [
      "+"; "-"; "*"; "/"; "<"; "="; ">";
      "append"; "apply"; "boolean?"; "car"; "cdr";
      "char->integer"; "char?"; "cons"; "cons*"; "denominator";
      "eq?"; "equal?"; "exact->inexact"; "flonum?"; "fold-left";
      "fold-right"; "gcd"; "integer?"; "integer->char"; "length";
      "list"; "list?"; "make-string"; "map"; "not"; "null?"; "number?";
      "numerator"; "pair?"; "procedure?"; "rational?"; "set-car!";
      "set-cdr!"; "string->list"; "string-length"; "string-ref";
      "string-set!"; "string?"; "symbol?"; "zero?"; "symbol->string"
    ] in
    if not debug then reserved_fvar_list_core
    else let debug_fvar_list = [
      "display"
    ] in
    reserved_fvar_list_core @ debug_fvar_list;;

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

  (*========== General code ==========*)

  let concat_list_of_code = fun lines ->
    let lines = if debug then lines
    else List.filter (fun line -> line <> "") lines in
    String.concat "\n" lines;;
  let generate_debug_nop = fun () -> if debug then "nop" else "";;
  let format_debug_label = fun append_nop name ->
    if debug then
      let label = name  ^ ":" in
      if append_nop then concat_list_of_code [label; "nop"]
      else label
    else "";;
  let format_debug_label_with_nop = format_debug_label true;;
  let format_debug_label_without_nop = format_debug_label false;;
  let format_label = fun append_nop prefix name ->
    format_debug_label append_nop (prefix ^ name);;
  let format_label_with_nop = format_label true;;
  let format_label_without_nop = format_label false;;

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

  let lambda_id_ref = ref(0);;
  let next_lambda_id = fun () ->
    lambda_id_ref := !lambda_id_ref + 1;
    !lambda_id_ref;;

  let lambda_label_prefix = ref(".lambda_global_");;
  let current_lambda_label_prefix = fun () -> !lambda_label_prefix;;
  let set_lambda_label_prefix = fun value -> lambda_label_prefix := value;;
  let format_lambda_label = fun name -> format_label_without_nop (current_lambda_label_prefix ()) name;;

  let enclosing_labmda_param_vars_ref = ref(0);;
  let get_enclosing_labmda_param_vars = fun () ->
    !enclosing_labmda_param_vars_ref;;
  let set_enclosing_labmda_param_vars = fun value ->
    enclosing_labmda_param_vars_ref := value;;

  let env_depth_ref = ref(-1);;
  let inc_env_depth () =  env_depth_ref := !env_depth_ref + 1;;
  let dec_env_depth () =  env_depth_ref := !env_depth_ref - 1;;

  let var_to_string = fun var ->
    match var with
    | VarFree(var_name) -> Printf.sprintf "VarFree %s" var_name
    | VarParam(var_name, minor) -> Printf.sprintf "VarParam %s, %d" var_name minor
    | VarBound (var_name, major, minor) ->
      Printf.sprintf "VarBound %s, major %d, minor %d" var_name major minor;;

  let expr'_to_string = expr'_to_scheme_code_lookalike;;
  let get_start_comment = fun s ->
    "; ---------- start of: " ^ s;;
  let get_end_comment = fun s ->
    "; ---------- end of: " ^ s;;

  let generate_code_wrapper = fun consts_tbl fvars expr' ->

    (*========== Const ==========*)

    let generate_code_for_constant = fun const ->
      let const_index = get_index_of_const_in_const_tbl const consts_tbl in
      let const_code_address = const_table_label ^ " + " ^ (string_of_int const_index) in
        Printf.sprintf "mov rax, %s  ; %s" const_code_address (constant_to_string const) in

    (*========== Vars get ==========*)

    let get_var_offset_code_from_fvars_tbl = fun var_name fvars ->
      let var_offset = get_index_in_fvars_tbl var_name fvars in
        fvar_tabel_label ^ " + " ^ (string_of_int var_offset) in

    let generate_code_for_free_var = fun var_name ->
      let comment = Printf.sprintf ";Get VarFree(%s)" var_name in
      let code_adress = get_var_offset_code_from_fvars_tbl var_name fvars in
      concat_list_of_code
      [
        "mov rax, qword [" ^ code_adress ^ "]" ^ "  " ^ comment
      ] in

    let generate_code_for_var_param = fun var_name minor ->
      let comment = Printf.sprintf ";Get VarParam(%s, %d)" var_name minor in
      concat_list_of_code
      [
        "mov rax, PVAR(" ^ (string_of_int minor) ^ ")" ^ "  " ^ comment
      ] in

    let generate_code_for_var_bound = fun var_name major minor ->
      let comment = Printf.sprintf ";Get VarBound(%s, %d, %d)" var_name major minor in
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
      | VarParam(var_name, minor) -> generate_code_for_var_param var_name minor
      | VarBound (var_name, major, minor) -> generate_code_for_var_bound var_name major minor in

    (*========== Vars set ==========*)

    let generate_code_for_free_var_set = fun var_name ->
      let comment = Printf.sprintf "Set VarFree(%s)" var_name in
      let var_address = get_var_offset_code_from_fvars_tbl var_name fvars in
      comment, concat_list_of_code
      [
        "mov qword [" ^ var_address ^ "], rax";
        "RET_VOID"
      ] in

    let generate_code_for_var_param_set = fun var_name minor ->
      let comment = Printf.sprintf "Set VarParam(%s, %d)" var_name minor in
      comment, concat_list_of_code
      [
        "mov PVAR(" ^ (string_of_int minor) ^ "), rax";
        "RET_VOID"
      ] in

    let generate_code_for_var_bound_set = fun var_name major minor ->
      let comment = Printf.sprintf "Set VarBound(%s, %d, %d)" var_name major minor in
      comment, concat_list_of_code
        [
          "mov rbx, ENV";
          "mov rbx, [rbx + WORD_SIZE * " ^ (string_of_int major) ^ "]";
          "mov qword [rbx + WORD_SIZE * " ^ (string_of_int minor) ^ "], rax";
          "RET_VOID"
        ] in

    let generate_code_for_set_var = fun var ->
      match var with
      | VarFree(var_name) -> generate_code_for_free_var_set var_name
      | VarParam(var_name, minor) -> generate_code_for_var_param_set var_name minor
      | VarBound (var_name, major, minor) -> generate_code_for_var_bound_set var_name major minor in

    let rec generate_code_for_set = fun var expr' ->
      let generated_code_for_expr' = generate_code expr' in
      let comment, generated_code_for_var' = generate_code_for_set_var var in
      comment, concat_list_of_code [generated_code_for_expr'; generated_code_for_var']

    (*========== Sequence ==========*)

    and generate_code_for_sequence = fun expr'_list ->
      let inner_comment_index = comment_index () in
      let omment = "sequence #" ^ inner_comment_index in
      let code_list = List.map generate_code expr'_list in
        omment, concat_list_of_code code_list

    (*========== If ==========*)

    and generate_code_for_if = fun test dit dif ->
      let inner_comment_index = comment_index () in
      let else_label_name = else_label_of_if () in
      let exit_label_name = exit_label () in
      let comment = "if #" ^ inner_comment_index in
      comment, concat_list_of_code
      [
        "; if test #" ^ inner_comment_index;
        generate_code test;
        "cmp rax, SOB_FALSE_ADDRESS";
        "je " ^ else_label_name;
        "; if dit #" ^ inner_comment_index;
        generate_code dit;
        "jmp " ^ exit_label_name;
        (else_label_name ^ ":");
        "; if dif #" ^ inner_comment_index;
        generate_code dif;
        (exit_label_name ^ ":")
      ]

    (*========== Or ==========*)

      and generate_code_for_or = fun expr'_list ->
        let inner_comment_index = comment_index () in
        let comment = "or #" ^ inner_comment_index in
        let exit_label_name = exit_label () in
        let cmp_and_jmp_code =
          concat_list_of_code
          [
            "cmp rax, SOB_FALSE_ADDRESS";
            "jne " ^ exit_label_name;
          ] in
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
          | [] -> ["mov rax, SOB_FALSE_ADDRESS"]
          | _ :: rest -> rest in
        let code_list = code_list @ [exit_label_name ^ ":"] in
        comment, concat_list_of_code code_list

    (*========== Box get set ==========*)

    and generate_code_for_box_get = fun var ->
      let inner_comment_index = comment_index () in
      "", concat_list_of_code
      [
        "; BoxGet #" ^ inner_comment_index ^ " of " ^  (var_to_string var);
        generate_code_for_var var;
        "mov rax, qword [rax]"
      ]

    and generate_code_for_box_set = fun var expr' ->
      let inner_comment_index = comment_index () in
      let comment = "; BoxSet #" ^ inner_comment_index ^ " of " ^  (var_to_string var) in
      comment, concat_list_of_code
      [
        generate_code expr';
         "push rax";
        generate_code_for_var var;
        "pop qword [rax]";
        "RET_VOID"
      ]

    and generate_code_for_box = fun var ->
      let inner_comment_index = comment_index () in
      "", concat_list_of_code
      [
        "; Box #" ^ inner_comment_index ^ " of " ^  (var_to_string var);
        generate_code_for_var var;
        "mov rbx, rax";
        "MALLOC rax, WORD_SIZE";
        "mov qword [rax], rbx"
      ]

    (*========== Define ==========*)

    and generate_code_for_def = fun var expr' ->
      let _, code = generate_code_for_set var expr' in
      "", code

    (*========== Lambda ==========*)

    and generate_code_for_lambda = fun kind body_expr' generate_body number_of_args  ->
      inc_env_depth ();

      let env_depth = !env_depth_ref in
      let enclosing_labmda_param_vars = get_enclosing_labmda_param_vars () in
      set_enclosing_labmda_param_vars number_of_args;

      let lambda_id = next_lambda_id () in
      let enclosing_label_prefix = current_lambda_label_prefix () in
      let enclosing_cont_index = comment_index () in
      let debug_labels_prefix = Printf.sprintf ".lambda_%s_%d_" kind lambda_id in
      set_lambda_label_prefix debug_labels_prefix;

      let lcode_label_name = Printf.sprintf "Lcode%d" lambda_id in
      let lcont_label_name = Printf.sprintf "Lcont%d" lambda_id in
      let code = concat_list_of_code
      [
        "; in depth " ^ (string_of_int env_depth);
        format_lambda_label "extend_env";
        "MALLOC rcx, WORD_SIZE*" ^ (string_of_int (env_depth+1));
        "mov rbx, ENV";
        "COPY_ARRAY_STATIC rbx, rcx, " ^ (string_of_int (env_depth)) ^ ", rax, 0, 1";
        format_lambda_label "copy_enclosing_params_to_env";
        "MALLOC rbx, WORD_SIZE*" ^ (string_of_int enclosing_labmda_param_vars);
        "mov qword [rcx], rbx";
        "PVAR_ADDR(rax, 0)";
        "COPY_ARRAY_STATIC rax, rbx, " ^ (string_of_int enclosing_labmda_param_vars) ^ ", rdx";
        format_lambda_label "make_closure";
        "MAKE_CLOSURE(rax, rcx, " ^ lcode_label_name ^ ")";
        format_lambda_label "skip_body";
        "jmp " ^ lcont_label_name;
        lcode_label_name ^ ":";
        "push rbp";
        "mov rbp, rsp";
        generate_body ();
        generate_code body_expr';
        "leave";
        "ret";
        lcont_label_name ^ ":";
        generate_debug_nop ();
        format_label_with_nop enclosing_label_prefix enclosing_cont_index
      ] in
      let comment = Printf.sprintf "lambda %s #%d with %d args" kind lambda_id number_of_args in

      set_lambda_label_prefix enclosing_label_prefix;
      set_enclosing_labmda_param_vars enclosing_labmda_param_vars;
      dec_env_depth ();
      comment, code

    and generate_code_for_lambda_simple = fun arg_names body_expr' ->
      let number_of_args = List.length arg_names in
      let body_fun_code = (fun () -> "") in
      generate_code_for_lambda "simple" body_expr' body_fun_code number_of_args

    and generate_code_for_lambda_opt = fun arg_names opt_arg_name body_expr' ->
      let number_of_required_args = List.length arg_names in
      let number_of_args = number_of_required_args + 1 in
      let body_fun_code =
      (fun () ->
        let format_debug_label = format_debug_label_without_nop in
        concat_list_of_code
        [
          format_debug_label ".check_if_opt_given";
          "mov r8, PARAMS_COUNT";
          "cmp r8, " ^ (string_of_int number_of_required_args);
          "je .no_opt_arg";

          format_debug_label ".with_opt_arg";
          "mov rbx, SOB_NIL_ADDRESS";
          "PVAR_ADDR(rsi, r8-1)";
          "PVAR_ADDR(rdi, " ^ (string_of_int (number_of_required_args - 1)) ^ ")";

          ".build_list_s:";
          "cmp rsi, rdi";
          "je .build_list_e";

          "mov rcx, qword [rsi]";
          "MAKE_PAIR(rax, rcx, rbx)";

          "mov rbx, rax";
          "sub rsi, WORD_SIZE";

          "jmp .build_list_s";
          ".build_list_e:";
          "nop";

          format_debug_label ".pull_stack_upwards";
          "mov PVAR(r8-1), rbx";
          "PVAR_ADDR(rsi, r8-2)";
          "PVAR_ADDR(rdi, " ^ (string_of_int (number_of_required_args - 1)) ^ ")";

          "mov r9, rsi";
          "sub r9, rdi";

          "mov rdx, " ^ (string_of_int (4 + number_of_required_args));
          "call copy_array_backward";

          format_debug_label ".fix_stack_pointers_with_opt";
          "add rsp, r9";
          "mov rbp, rsp";

          "jmp .stack_adjustment_done";
          ".no_opt_arg:";
          generate_debug_nop ();
          format_debug_label ".pull_stack_one_downwards";
          "lea rsi, [rbp - WORD_SIZE]";
          "COPY_ARRAY_STATIC rbp, rsi, " ^ (string_of_int (4 + number_of_required_args)) ^ ", rcx";
          format_debug_label ".fix_stack_pointers_no_opt";
          "mov rbp, rsi";
          "mov rsp, rbp";
          format_debug_label ".set_opt_nil";
          "mov PVAR(" ^ (string_of_int number_of_required_args) ^ "), SOB_NIL_ADDRESS";
          ".stack_adjustment_done:";
          "mov PARAMS_COUNT, " ^ (string_of_int number_of_args)
        ]
      ) in
      generate_code_for_lambda "opt" body_expr' body_fun_code number_of_args

    (*========== Applic ==========*)

    and generate_code_for_applic_core = fun label_prefix operator_expr' operands_expr'_list ->
      let format_label_with_nop = format_label_with_nop label_prefix in
      let format_label_without_nop = format_label_without_nop label_prefix in

      let prepare_frame =
        let start_label = format_label_with_nop "push_args" in
        let arg_index_ref = ref(0) in
        let push_args =
          List.fold_left
          (fun acc expr' ->
            let push_arg_code = [
              format_label_with_nop (Printf.sprintf "push_arg_%d" !arg_index_ref);
              generate_code expr';
              "push rax"
            ] in
            arg_index_ref := !arg_index_ref + 1;
            push_arg_code @ acc )
          []
          operands_expr'_list in
        let push_rest = [
          format_label_without_nop "push_args_count";
          "push " ^ (string_of_int (List.length operands_expr'_list));
          format_label_with_nop "operator_code";
          generate_code operator_expr';
          format_label_without_nop "push_env";
          "push qword [rax + TYPE_SIZE]"
        ] in
        start_label :: (push_args @ push_rest) in
      let frame_cleanup = [
        format_label_without_nop "frame_cleanup";
        "add rsp, WORD_SIZE  ; pop env";
        "pop rbx  ; pop args count";
        "shl rbx, 3  ; rbx = rbx * 8";
        "add rsp, rbx  ; pop args"
      ] in
      (prepare_frame, frame_cleanup)

    and generate_code_for_applic = fun operator_expr' operands_expr'_list ->
      let applic_id = comment_index () in
      let comment = Printf.sprintf "applic #" ^ applic_id in
      let debug_label_prefix = Printf.sprintf ".applic_normal_%s_" applic_id in
      let format_label = format_label_without_nop debug_label_prefix in

      let (prepare_frame, frame_cleanup) = generate_code_for_applic_core debug_label_prefix operator_expr' operands_expr'_list in
      let call_code = [
        format_label "call_op";
        "call [rax + TYPE_SIZE + WORD_SIZE]"
      ] in
      comment, concat_list_of_code (prepare_frame @ call_code @ frame_cleanup)

    and generate_code_for_applictp = fun operator_expr' operands_expr'_list ->
      let applic_id = comment_index () in
      let comment = Printf.sprintf "applic TP #" ^ applic_id in
      let debug_label_prefix = Printf.sprintf ".applic_TP_%s_" applic_id in
      let format_label = format_label_without_nop debug_label_prefix in

      let (prepare_frame, frame_cleanup) = generate_code_for_applic_core debug_label_prefix operator_expr' operands_expr'_list in
      let call_code =
        let enclosing_labmda_param_vars = get_enclosing_labmda_param_vars () in
        let operands_amount = List.length operands_expr'_list in
        let new_frame_size = operands_amount + 4 in [
          format_label "fix_frame";
          "push RET_ADDR  ;push old ret address";
          "push OLD_RBP ;save the old RBP pointer";
          format_label "copy_new_frame";
          "lea rbx, [rbp - WORD_SIZE] ;set rbx to point the top of the new frame";
          "lea rsp, [rbp + WORD_SIZE*(3 + " ^ (string_of_int enclosing_labmda_param_vars) ^ ")] ; set rsp to point to the top of the old frame";
          "COPY_ARRAY_STATIC rbx, rsp, " ^ (string_of_int new_frame_size) ^ ", rcx, 0, 0, -1, -1";
          format_label "fix_frame_pointers";
          "lea rsp, [rbp+WORD_SIZE*(" ^ (string_of_int (-(operands_amount - enclosing_labmda_param_vars) + 1)) ^ ")] ;setup the stack pointer for the new function. it point to the return address right now";
          "mov rbp, [rsp - WORD_SIZE] ;restore the old RBP pointer";
          format_label "call_op";
          "jmp [rax + TYPE_SIZE + WORD_SIZE]"
        ] in
      comment, concat_list_of_code (prepare_frame @ call_code @ frame_cleanup)

    (*========== Generate code ==========*)
    and generate_code = fun expr' ->
      if not debug then
        let _, code = generate_code_core expr' in
        code
      else match expr' with
      | (Const' _ | Var' _) ->
        let _, code = generate_code_core expr' in
        code
      | _ ->
        let expr'_scheme_code = expr'_to_string expr' in
        let start_comment = get_start_comment expr'_scheme_code in
        let end_comment = get_end_comment expr'_scheme_code in
        let comment, asm = generate_code_core expr' in
        let code_list =
          if comment = "" then [start_comment; asm; end_comment]
          else
            let expr_start_comment = "; start: " ^ comment in
            let expr_end_comment = "; end: " ^ comment in
            [start_comment; expr_start_comment; asm; expr_end_comment; end_comment] in
        concat_list_of_code code_list

    and generate_code_core = fun expr' ->
      match expr' with
      | Const'(const) -> "", generate_code_for_constant const

      | Var'(var) -> "", generate_code_for_var var
      | Box'(var) -> generate_code_for_box var
      | BoxGet'(var) -> generate_code_for_box_get var
      | BoxSet'(var, expr') -> generate_code_for_box_set var expr'

      | If'(test, dit, dif) -> generate_code_for_if test dit dif

      | Seq'(expr'_list) -> generate_code_for_sequence expr'_list

      | Set'(var, expr') -> generate_code_for_set var expr'
      | Def'(var, expr') -> generate_code_for_def var expr'

      | Or'(expr'_list) -> generate_code_for_or expr'_list

      | LambdaSimple'(arg_names, body_expr') -> generate_code_for_lambda_simple arg_names body_expr'
      | LambdaOpt'(req_arg_names, opt_arg_name, body_expr') -> generate_code_for_lambda_opt req_arg_names opt_arg_name body_expr'

      | Applic'(operator_expr', operands_expr'_list) -> generate_code_for_applic operator_expr' operands_expr'_list
      | ApplicTP'(operator_expr', operands_expr'_list) -> generate_code_for_applictp operator_expr' operands_expr'_list in
    generate_code expr';;

  let make_consts_tbl = fun asts ->
    build_const_tbl asts

  let make_fvars_tbl asts =
    build_fvars_tbl asts;;

  let generate consts fvars e =
    generate_code_wrapper consts fvars e;;
end;;
