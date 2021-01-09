#use "semantic-analyser.ml";;

module type EXP_STRING = sig
  val constant_to_string : constant -> string
  val sexpr_to_string : sexpr -> string
  val expr'_to_scheme_code_lookalike : expr' -> string
end;;

module ExpString : EXP_STRING = struct

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
      | _ -> (
        let char_code = Char.code c in
        if char_code < 32 then
          let hex = Printf.sprintf "\\x%02x" char_code in
          string_to_list hex
        else [c]
      ) in
      l @ acc)
    chars_list
    [] in
  list_to_string chars_list;;

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
  | String s -> Printf.sprintf "\"%s\"" (sexpr_string_to_string s)
  | Symbol s -> Printf.sprintf "'%s" s
  | (Pair _ as pair) -> sexpr_pair_to_string pair;;

let constant_to_string = function
  | Sexpr sexpr -> sexpr_to_string sexpr
  | Void -> "#<void>";;

let var_name = fun var ->
  match var with
  | VarFree var_name -> var_name
  | VarParam (var_name, var_pos) -> var_name
  | VarBound (var_name, major, minor) -> var_name;;

let expr'_to_scheme_code_lookalike = fun expr' ->
  let rec expr'_list_to_string_core = fun expr's_list ->
    let (s, _) =
      List.fold_right
      (fun expr' (acc, is_last) ->
          let expr'_string = expr'_to_string expr' in
          let new_acc =
          if is_last then expr'_string
          else Printf.sprintf "%s %s" expr'_string acc in
          (new_acc, false))
      expr's_list
      ("", true) in
    s

  and expr'_list_to_string_with_expr_name = fun name expr's_list ->
    Printf.sprintf "(%s %s)" name (expr'_list_to_string_core expr's_list)

  and expr'_list_to_string = fun expr's_list ->
    Printf.sprintf "(%s)" (expr'_list_to_string_core expr's_list)

  and lambda_to_stirng = fun args_names opt_arg_name_opt expr' ->
    let args_list_s =
      let args_list_s = String.concat " " args_names in
      match args_names, opt_arg_name_opt with
      | [], Some opt_arg_name -> opt_arg_name
      | _, Some opt_arg_name -> Printf.sprintf "(%s . %s)" args_list_s opt_arg_name
      | _ -> Printf.sprintf "(%s)" args_list_s in
    let expr'_s = expr'_to_string expr' in
    Printf.sprintf "(lambda %s %s)" args_list_s expr'_s

  and applic_to_string = fun expr' expr'_list ->
    expr'_list_to_string (expr' :: expr'_list)

  and expr'_to_string = function
  | Const' const -> constant_to_string const
  | Var' var -> var_name var
  | Box' var -> Printf.sprintf "<Box>(%s)" (var_name var)
  | BoxGet' var -> Printf.sprintf "<Box>(%s)" (var_name var)
  | BoxSet' (var, expr') -> Printf.sprintf "(set! <Box>(%s) %s)" (var_name var) (expr'_to_string expr')
  | If' (test, dit, dif) -> expr'_list_to_string_with_expr_name "if" [test; dit; dif]
  | Seq' expr'_list -> expr'_list_to_string_with_expr_name "begin" expr'_list
  | Set' (var, expr') -> Printf.sprintf "(set! %s %s)" (var_name var) (expr'_to_string expr')
  | Def' (var, expr') -> Printf.sprintf "(define %s %s)" (var_name var) (expr'_to_string expr')
  | Or' expr'_list -> expr'_list_to_string_with_expr_name "or" expr'_list

  | LambdaSimple' (args_list, expr') -> lambda_to_stirng args_list None expr'
  | LambdaOpt' (args_list, arg_opt, expr') -> lambda_to_stirng args_list (Some arg_opt) expr'
  | Applic' (expr', expr'_list) -> applic_to_string expr' expr'_list
  | ApplicTP' (expr', expr'_list) -> Printf.sprintf "<TP>%s" (applic_to_string expr' expr'_list) in

  expr'_to_string expr';;
end;;
