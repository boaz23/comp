#use "compiler.ml"

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

(* let a = Code_Gen.make_fvars_tbl (string_to_asts "(define a 1)");; *)

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
