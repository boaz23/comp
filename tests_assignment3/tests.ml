#use "semantic-analyser.ml";;

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
  | X_syntax_error -> Printf.printf "Tag parser got error at { %s }\n" string

let read_expr' = fun f string ->
  read_expr
    (fun expr ->
      try (
        let expr' = Semantics.annotate_lexical_addresses expr in
        f expr'
      )
      with X_syntax_error -> Printf.printf "Semantics analysis got error at { %s }\n" string)
    string

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
  | c -> String.make 1 c

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
  | (Pair _ as pair) -> sexpr_pair_to_string pair

let constant_to_string = function
  | Sexpr sexpr -> Printf.sprintf "%s\n" (sexpr_to_string sexpr)
  | Void -> "Void";;

let var_to_string = fun var ->
  let s = match var with
  | VarFree var_name -> Printf.sprintf "VarFree %s" var_name
  | VarParam (var_name, var_pos) -> Printf.sprintf "VarParam (%s, %d))" var_name var_pos
  | VarBound (var_name, major, minor) -> Printf.sprintf "VarBound (%s, %d, %d)" var_name major minor in
  Printf.sprintf "Var' %s" s

let rec expr'_list_to_string = fun expr's_list ->
  let (s, _) =
    List.fold_right
      (fun expr' (acc, is_last) ->
        let expr'_string = expr'_to_string expr' in
        let new_acc =
          if is_last then expr'_string
          else Printf.sprintf "%s; %s" acc expr'_string in
        (new_acc, false))
      expr's_list
      ("", true) in
  Printf.sprintf "[%s]" s

and lambda_to_stirng = fun lambda_name args_list arg_opt expr' ->
    let args_list_s = String.concat "; " args_list in
    let args_list_s = "[" ^ args_list_s ^ "]" in
    let expr'_s = expr'_to_string expr' in
    let arg_opt_s =
      if arg_opt <> ""
      then ", " ^ arg_opt
      else "" in
    Printf.sprintf "(%s%s, (%s))" args_list_s arg_opt_s expr'_s

and applic_to_string = fun applic_name expr' expr'_list ->
  let expr'_s = expr'_to_string expr' in
  let expr'_list_s = expr'_list_to_string expr'_list in
  Printf.sprintf "%s (%s, %s)" applic_name expr'_s expr'_list_s

and expr'_to_string = function
  | Const' const -> Printf.sprintf "Const' %s" (constant_to_string const)
  | Var' var -> var_to_string var
  | Box' var -> Printf.sprintf "Box' %s" (var_to_string var)
  | BoxGet' var -> Printf.sprintf "BoxGet' %s" (var_to_string var)
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

let make_abstract_test_analysis_from_string = fun f_transformation f_out_transformation f_report_error string expected ->
  read_expr'
    (fun expr' ->
      let actual = f_transformation expr' in
      if (actual <> expected) then
        let actual_t = f_out_transformation actual in
        let expected_t = f_out_transformation expected in
        f_report_error string actual_t expected_t)
    string

let make_test_analysis_from_string = fun f string expected ->
  make_abstract_test_analysis_from_string
    f
    expr'_to_string
    (fun s actual_s expected_s ->
      Printf.printf
        "error in test { %s }\n  expected: { %s }\n  actual: { %s }\n"
        s
        expected_s
        actual_s)
    string
    expected

let test_annotate_lexical_addresses = fun string expected ->
  make_test_analysis_from_string (fun expr' -> expr') string expected