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
   then Printf.sprintf "Error in code:\n %s" code
   else Printf.sprintf "";;

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
       (Sexpr (Symbol "ab"), (104, "MAKE_LITERAL_SYMBOL(const_tbl+6)"))];;