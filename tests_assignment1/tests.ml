#use "reader.ml";;

let read_sexpr = fun f string ->
  try (
    let sexprs = Reader.read_sexprs string in
    f sexprs
  )
  with
  | PC.X_no_match -> Printf.printf "Reader got error at { %s }\n" string
  | X_not_yet_implemented -> Printf.printf "Function is not yet implemented\n";;

let test_reader string expected =
  read_sexpr
    (fun read_sexprs ->
      let valid =
        if List.compare_lengths read_sexprs expected <> 0 then false
        else if List.exists2
          (fun expected_sexpr read_sexpr -> read_sexpr <> expected_sexpr)
          expected
          read_sexprs then false
        else true in
      if not valid then Printf.printf "reader failed on { %s }\n" string)
    string;;
let test_read_invalid_input string =
  try let _ = Reader.read_sexprs string in
    Printf.printf "reader failed on { %s }\n" string
  with PC.X_no_match -> ();;


let main () =
  Printf.printf "\nrunning tests...\n";
  test_reader "1" [Number (Fraction (1, 1))];
  test_reader "  1" [Number (Fraction (1, 1))];
  test_reader "  1  " [Number (Fraction (1, 1))];
  test_reader "01234" [Number (Fraction (1234, 1))];
  test_reader "001234" [Number (Fraction (1234, 1))];
  test_reader "-12" [Number (Fraction (-12, 1))];
  test_reader "-012" [Number (Fraction (-12, 1))];
  test_reader "+1234" [Number (Fraction (1234, 1))];
  test_reader "+00940" [Number (Fraction (940, 1))];
  test_reader "1/1" [Number (Fraction (1, 1))];
  test_reader "2/4" [Number (Fraction (1, 2))];
  test_reader "-17/6" [Number (Fraction (-17, 6))];
  test_reader "+006/012" [Number (Fraction (1, 2))];

  test_reader "+1e1" [Number (Float 10.0)];
  test_reader "1E+1" [Number (Float 10.0)];
  test_reader "10e-1" [Number (Float 1.0)];
  test_reader "3.14e+9" [Number (Float 3.14e+9)];
  test_reader "3.14E-512" [Number (Float 3.14e-512)];
  test_reader "+0000012.3E0000000000002" [Number (Float 12.3e+2)];
  test_reader "-5.000000e-2" [Number (Float (-5.0e-2))];
  test_reader "+5.000000e1" [Number (Float 5.0e+1)];

  test_reader "1.0" [Number (Float 1.0)];
  test_reader "  1.0  " [Number (Float 1.0)];
  test_reader "005.01290" [Number (Float 5.0129)];
  test_reader "501.000000" [Number (Float 501.0)];
  test_reader "+999.009000" [Number (Float 999.009)];
  test_reader "-001.000123000" [Number (Float (-1.000123))];

  test_reader "#t" [Bool true];
  test_reader "#f" [Bool false];
  test_reader "#t#t" [Bool true; Bool true];
  test_reader " #t #t" [Bool true; Bool true];

  test_reader "1a^" [Symbol "1a^"];
  test_reader " 1a^   " [Symbol "1a^"];
  test_reader "1a<:" [Symbol "1a<:"];
  test_reader "AbC" [Symbol "abc"];
  test_reader "a1+3====1.1" [Symbol "a1+3====1.1"];
  test_reader ".." [Symbol ".."];
  test_reader "..123Ac^" [Symbol "..123ac^"];
  test_reader "..123ac^" [Symbol "..123ac^"];

  test_reader "\"\"" [String ""];
  test_reader "\"    \"" [String "    "];
  test_reader "   \"\"    " [String ""];
  test_reader "\"hello . ^ . \"" [String "hello . ^ . "];
  test_reader "\"hello . ^ [] ; a {} #@\"" [String "hello . ^ [] ; a {} #@"];
  test_reader "\"\\r\"" [String "\r"];
  test_reader "\"\\n\"" [String "\n"];
  test_reader "\"\\t\"" [String "\t"];
  test_reader "\"\\f\"" [String "\012"];
  test_reader "\"\\\\\"" [String "\\"];

  test_reader "#\\a" [Char 'a'];
  test_reader "   #\\a " [Char 'a'];
  test_reader "#\\A" [Char 'A'];
  test_reader "#\\nul" [Char '\000'];
  test_reader "#\\newline" [Char '\n'];
  test_reader "#\\return" [Char '\r'];
  test_reader "#\\tab" [Char '\t'];
  test_reader "#\\page" [Char '\012'];
  test_reader "#\\space" [Char ' '];

  test_reader "'1" [Pair (Symbol "quote", Pair (Number (Fraction (1, 1)), Nil))];
  test_reader "'3+2" [Pair (Symbol "quote", Pair (Symbol "3+2", Nil))];
  test_reader "'(a 1 . a)" [Pair (Symbol "quote", Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Symbol "a")), Nil))];

  test_reader "`1" [Pair (Symbol "quasiquote", Pair (Number (Fraction (1, 1)), Nil))];
  test_reader "`3+2" [Pair (Symbol "quasiquote", Pair (Symbol "3+2", Nil))];
  test_reader "`(a 1 . a)" [Pair (Symbol "quasiquote", Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Symbol "a")), Nil))];

  test_reader ",1" [Pair (Symbol "unquote", Pair (Number (Fraction (1, 1)), Nil))];
  test_reader ",3+2" [Pair (Symbol "unquote", Pair (Symbol "3+2", Nil))];
  test_reader ",(a 1 . a)" [Pair (Symbol "unquote", Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Symbol "a")), Nil))];

  test_reader "`'(1)" [Pair (Symbol "quasiquote", Pair (Pair (Symbol "quote", Pair (Pair (Number (Fraction (1, 1)), Nil), Nil)), Nil))];
  test_reader "`(1 ,@(+ 1 2))" [
    Pair (Symbol "quasiquote",
      Pair
        (Pair (Number (Fraction (1, 1)),
          Pair
            (Pair (Symbol "unquote-splicing",
              Pair
                (Pair (Symbol "+",
                  Pair (Number (Fraction (1, 1)),
                    Pair (Number (Fraction (2, 1)), Nil))),
                Nil)),
            Nil)),
        Nil))
  ];

  test_reader "(   1 )" [Pair (Number (Fraction (1, 1)), Nil)];
  test_reader "(1 2 \"a\")" [Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (String "a", Nil)))];
  test_reader "(1aC +2.3e1 \"a\")" [Pair (Symbol "1ac", Pair (Number (Float 23.0), Pair (String "a", Nil)))];

  test_reader "(1 . 2)" [Pair (Number (Fraction (1, 1)), Number (Fraction (2, 1)))];
  test_reader "(#t . #\\A)" [Pair (Bool true, Char 'A')];
  test_reader "(\"#tab hello\" #t . #\\A)" [Pair (String "#tab hello", Pair (Bool true, Char 'A'))];
  test_reader "(a a a)" [Pair (Symbol "a", Pair (Symbol "a", Pair (Symbol "a", Nil)))];

  test_reader "(a a a `(1 . '(#\\tab abC . 3))) (+ 1 2)" [
    Pair (Symbol "a", Pair (Symbol "a", Pair (Symbol "a",
      Pair (
        Pair (Symbol "quasiquote", Pair (
          Pair (Number (Fraction (1, 1)),
            Pair (Symbol "quote", Pair (
              Pair (Char '\t', Pair (Symbol "abc", Number (Fraction (3, 1)))),
            Nil))
          ),
        Nil)),
    Nil))));

    Pair (Symbol "+", Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Nil)))];

  test_reader "()" [Nil];
  test_reader "(    )" [Nil];
  test_reader "( ;comment\n ;comment\n)" [Nil];
  test_reader "( ;comment\n ;comment\n a)" [Pair (Symbol "a", Nil)];
  test_reader "( ;comment\n ;comment\n #;(+ 1 2) a)" [Pair (Symbol "a", Nil)];
  test_reader "    ;commnet\n #;(()) '  ;c\n  #;5.2 hello " [Pair (Symbol "quote", Pair (Symbol "hello", Nil))];

  test_reader "(a);quasiqute next line\n`(a 'hello . ,@(+ 1 2))" [
    Pair (Symbol "a", Nil);
    Pair (Symbol "quasiquote", Pair (
      Pair (Symbol "a",
        Pair (
          Pair (Symbol "quote", Pair (Symbol "hello", Nil)),
          Pair (Symbol "unquote-splicing",
            Pair (Pair (Symbol "+", Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Nil))), Nil))
        )
      ),
    Nil))];
  test_reader "  ;\n   #;() (   ;commnet\n #;(()) '  ;c\n  #;5.2 hello  #;1 ;;;;\n   #;aaa bbb\"aa\"#t  .  64.0 ;ccc\n #;aa ) "
    [Pair (Pair (Symbol "quote", Pair (Symbol "hello", Nil)), Pair (Symbol "bbb", Pair (String "aa", Pair (Bool true, Number (Float 64.0)))))];
  test_reader "  ; \n #;11 (     #;#;#;aa\"a\"(#t#f\"b\")    ;gggg\n   ;\n #;1 ;\n    ;;;\n ;\n #;#;a 11 )" [Nil];

  test_read_invalid_input ".";
  test_read_invalid_input "#ta";
  test_read_invalid_input "\"\\\"";
  test_read_invalid_input "(1 . 2 . 3)";;

main();;