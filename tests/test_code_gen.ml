#use "compiler.ml"

let read_expr = fun f string ->
  try (
    let sexprs = Reader.read_sexprs string in
    let exprs_ast = Tag_Parser.tag_parse_expressions sexprs in
    match exprs_ast with
    | expr :: [] -> f expr
    | _ -> Printf.printf "The following string has multiple expressions: { %s }\n" string
  )
  with
  | PC.X_no_match -> Printf.printf "Reader got error at { %s }\n" string
  | X_not_yet_implemented -> Printf.printf "Function is not yet implemented\n";;

let read_expr' = fun f string ->
  read_expr
    (fun expr ->
      try (
        let expr' = Semantics.annotate_lexical_addresses expr in
        f expr'
      )
      with X_syntax_error -> Printf.printf "Semantics analysis got error at { %s }\n" string)
    string;;

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



let string_to_asts s = List.map Semantics.run_semantics
                         (Tag_Parser.tag_parse_expressions
                            (Reader.read_sexprs s));;

let test_const_tbl_eq = fun asts expected_const_tbl ->
   let generated_const_tbl = Code_Gen.make_consts_tbl asts in
      List.fold_left2
      (fun bool_acc generated_const expected_const ->
         if bool_acc
         then generated_const = expected_const
         else true
      )
      true
      generated_const_tbl
      expected_const_tbl;;

let test_const_tbl = fun code expected_const_tbl ->
   let asts = string_to_asts code in
   let is_same_tbl = test_const_tbl_eq asts expected_const_tbl in
   if is_same_tbl = false
   then (
      let e_message = Printf.sprintf "Error in code:\n %s" code in
      print_string e_message
   )
   else ();;

let test_code_gen = fun expr' ->
   let const_tbl = Code_Gen.make_consts_tbl [expr'] in
   let fvars = Code_Gen.make_fvars_tbl [expr'] in
   let generate = Code_Gen.generate const_tbl fvars in
   let generated_code = generate expr' in
   let e_message = Printf.sprintf "code:\n %s\ngenerate:\n%s\n"
                     (expr'_to_string expr')
                        generated_code in
   print_string e_message


let a = Code_Gen.make_fvars_tbl (string_to_asts "(define a 1)");;

let consts_tbl_tests() =

   test_const_tbl "(list \"ab\" '(1 2) 'c 'ab)"
      [(Void, (0, "db T_VOID"));
       (Sexpr Nil, (1, "db T_NIL"));
       (Sexpr (Bool false), (2, "db T_BOOL, 0"));
       (Sexpr (Bool true), (4, "db T_BOOL, 1"));
       (Sexpr (String "ab"), (6, "MAKE_LITERAL_STRING(2, \"ab\")"));
       (Sexpr (Number (Fraction (1, 1))), (17, "MAKE_LITERAL_RATIONAL(1, 1)"));
       (Sexpr (Number (Fraction (2, 1))), (34, "MAKE_LITERAL_RATIONAL(2, 1)"));
       (Sexpr (Pair (Number (Fraction (2, 1)), Nil)),
        (51, "MAKE_LITERAL_PAIR(const_tbl+34, const_tbl+1)"));
       (Sexpr
         (Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Nil))),
        (68, "MAKE_LITERAL_PAIR(const_tbl+17, const_tbl+51)"));
       (Sexpr (String "c"), (85, "MAKE_LITERAL_STRING(1, \"c\")"));
       (Sexpr (Symbol "c"), (95, "MAKE_LITERAL_SYMBOL(const_tbl+85)"));
       (Sexpr (Symbol "ab"), (104, "MAKE_LITERAL_SYMBOL(const_tbl+6)"))];

   test_const_tbl "(define add (lambda (a b) ((display '(+ a b)) (+ a b) ))) (add 1 9)"
      [(Void, (0, "db T_VOID"));
      (Sexpr Nil, (1, "db T_NIL"));
      (Sexpr (Bool false), (2, "db T_BOOL, 0"));
      (Sexpr (Bool true), (4, "db T_BOOL, 1"));
      (Sexpr (String "+"), (6, "MAKE_LITERAL_STRING(1, \"+\")"));
      (Sexpr (Symbol "+"), (16, "MAKE_LITERAL_SYMBOL(const_tbl+6)"));
      (Sexpr (String "a"), (25, "MAKE_LITERAL_STRING(1, \"a\")"));
      (Sexpr (Symbol "a"), (35, "MAKE_LITERAL_SYMBOL(const_tbl+25)"));
      (Sexpr (String "b"), (44, "MAKE_LITERAL_STRING(1, \"b\")"));
      (Sexpr (Symbol "b"), (54, "MAKE_LITERAL_SYMBOL(const_tbl+44)"));
      (Sexpr (Pair (Symbol "b", Nil)),
      (63, "MAKE_LITERAL_PAIR(const_tbl+54, const_tbl+1)"));
      (Sexpr (Pair (Symbol "a", Pair (Symbol "b", Nil))),
      (80, "MAKE_LITERAL_PAIR(const_tbl+35, const_tbl+63)"));
      (Sexpr (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil)))),
      (97, "MAKE_LITERAL_PAIR(const_tbl+16, const_tbl+80)"));
      (Sexpr (Number (Fraction (1, 1))), (114, "MAKE_LITERAL_RATIONAL(1, 1)"));
      (Sexpr (Number (Fraction (9, 1))), (131, "MAKE_LITERAL_RATIONAL(9, 1)"))];;

let print_code_gen_for_vars() =
   test_code_gen (Var'(VarFree("var")));
   test_code_gen (Var'(VarParam("var", 2)));
   test_code_gen (Var'(VarBound("var", 1, 2)));;

let print_code_gen_for_set_vars() =
   test_code_gen (Set'( (VarFree("var")), (Const'(Sexpr (Number (Fraction (1, 1)))))));
   test_code_gen (Set'( (VarParam("var", 2)), (Const'(Sexpr (Number (Fraction (1, 1)))))));
   test_code_gen (Set'( (VarBound("var", 1, 2)), (Const'(Sexpr (Number (Fraction (1, 1)))))));;