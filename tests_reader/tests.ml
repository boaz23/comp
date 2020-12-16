#use "reader.ml";;

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

let sexpr_list_to_string sexprs =
  let string = match sexprs with
  | [] -> ""
  | sexpr :: [] -> sexpr_to_string sexpr
  | sexpr :: rest ->
    let first_string = sexpr_to_string sexpr in
    List.fold_left
      (fun acc sexpr -> acc ^ "; " ^ (sexpr_to_string sexpr))
      first_string
      sexprs in
  Printf.sprintf "[%s]" string;;

let read_sexpr = fun f string ->
  try (
    let sexprs = Reader.read_sexprs string in
    f sexprs
  )
  with
  | PC.X_no_match -> Printf.printf "Reader got error at {%s}\n" string
  | X_not_yet_implemented -> Printf.printf "Function is not yet implemented\n";;

let test_reader string expected =
  read_sexpr
    (fun read_sexprs ->
      let expected_string = sexpr_list_to_string expected in
      let actual_string = sexpr_list_to_string read_sexprs in
        if List.compare_lengths read_sexprs expected <> 0
        then Printf.printf "reader failed on {%s}\n  expected: {%s}\n  actual: {%s}\n" string expected_string actual_string
        else List.iter2
          (fun expected_sexpr read_sexpr ->
            if read_sexpr <> expected_sexpr
            then Printf.printf "reader failed on {%s}\n  expected: {%s}\n  actual: {%s}\n" string (sexpr_to_string expected_sexpr) (sexpr_to_string read_sexpr))
          expected
          read_sexprs)
    string;;
let test_read_invalid_input string =
  try let _ = Reader.read_sexprs string in
    Printf.printf "reader return a result while expected a no match exception on {%s}\n" string
  with PC.X_no_match -> ();;

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

let our_tests () =
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

let official_tests () =
  Printf.printf "official tests...\n";
  test_reader (* 1 *) "#t\n" [Bool true];
  test_reader (* 2 *) "#F\n" [Bool false];
  test_reader (* 3 *) "#\\newline\n" [Char '\n'];
  test_reader (* 4 *) "#\\RETURN\n" [Char '\r'];
  test_reader (* 5 *) "#\\Nul\n" [Char '\000'];
  test_reader (* 6 *) "#\\a\n" [Char 'a'];
  test_reader (* 7 *) "#\\?\n" [Char '?'];
  test_reader (* 8 *) "#\\A b\n" [Char 'A'; Symbol "b"];
  test_reader (* 9 *) "1\n" [Number (Fraction(1, 1))];
  test_reader (* 10 *) "  -123\n" [Number (Fraction (-123, 1))];
  test_reader (* 11 *) "0001\n" [Number (Fraction (1, 1))];
  test_reader (* 12 *) "1.2\n" [Number (Float 1.2)];
  test_reader (* 13 *) "-0.9\n" [Number (Float (-0.9))];
  test_reader (* 14 *) "+1.23456789\n" [Number (Float 1.23456789)];
  test_reader (* 15 *) "1   2\n" [Number (Fraction (1, 1)); Number (Fraction (2, 1))];
  test_reader (* 16 *) "0.1    0.2\n" [Number (Float 0.1); Number (Float 0.2)];
  test_reader (* 17 *) "9.9 ;comment\\n\n" [Number (Float 9.9)];
  test_reader (* 21 *) "10e-1\n" [Number (Float 1.)];
  test_reader (* 22 *) "1e1\n" [Number (Float 10.)];
  test_reader (* 23 *) "3.14e+9\n" [Number (Float 3140000000.)];
  test_reader (* 24 *) "1 ;HelloWorld\\n\n" [Number (Fraction (1, 1))];
  test_reader (* 25 *) "(;Is it List?\n)\n" [Nil];
  test_reader (* 26 *) "(    #;#t    )\n" [Nil];
  test_reader (* 27 *) " #f       ;dfkdsfjdisf dkfmk43rtmsdkfmzcpc3-33#@%#$^$##@#@!#" [Bool false];
  test_reader (* 28 *) "(1 2)\n" [Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Nil))];
  test_reader (* 29 *) "(1.2 . 3)\n" [Pair (Number (Float 1.2), Number (Fraction (3, 1)))];
  test_reader (* 30 *) "((1 2) . 3)\n" [Pair (Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Nil)), Number (Fraction (3, 1)))];
  test_reader (* 31 *) "(a . b)\n" [Pair (Symbol "a", Symbol "b")];
  test_reader (* 32 *) "(a b c)\n" [Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))];
  test_reader (* 33 *) "(a b . c)\n" [Pair (Symbol "a", Pair (Symbol "b", Symbol "c"))];
  test_reader (* 34 *) "(a . (b . (c . (d . e))))\n" [Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Pair (Symbol "d", Symbol "e"))))];
  test_reader (* 35 *) "((1 (a)) b)\n" [Pair (Pair (Number (Fraction (1, 1)), Pair (Pair (Symbol "a", Nil), Nil)),Pair (Symbol "b", Nil))];
  test_reader (* 36 *) "(\"str\" ; comment \n)\n" [Pair (String "str", Nil)];
  test_reader (* 37 *) "\"Hello World\"\n" [String "Hello World"];
  test_reader (* 38 *) "\"Hello \\t \\r \\n world!\"\n" [String "Hello \t \r \n world!"];
  test_reader (* 39 *) "\"Hello \\\\t \\\\r \\n world!\"\n" [String "Hello \\t \\r \n world!"];
  test_reader (* 40 *) "\"  Hi  \"\n" [String "  Hi  "];
  test_reader (* 41 *) "123abc\n" [Symbol "123abc"];
  test_reader (* 42 *) "123AbC\n" [Symbol "123abc"];
  test_reader (* 43 *) "$Dollar\n" [Symbol "$dollar"];
  test_reader (* 44 *) "!$HI\n" [Symbol "!$hi"];
  test_reader (* 57 *) "(#; 1 (2 #; 3) #; (1 2) 3 #; #; (1 #; 3) 2 3)\n" [Pair (Pair (Number (Fraction (2, 1)), Nil),Pair (Number (Fraction (3, 1)), Pair (Number (Fraction (3, 1)), Nil)))];
  test_reader (* 58 *) "'a\n" [Pair (Symbol "quote", Pair (Symbol "a", Nil))];
  test_reader (* 59 *) "`(1 ;asd\n 2 3 #;#;#;123 2 3)\n" [Pair (Symbol "quasiquote",Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Nil))];
  test_reader (* 60 *) ",\"string\"\n" [Pair (Symbol "unquote", Pair (String "string", Nil))];
  test_reader (* 61 *) ",@123\n" [Pair (Symbol "unquote-splicing", Pair (Number (Fraction (123, 1)), Nil))];
  test_reader (* 62 *) "(a . (b #;#t . ( (c . d) . e)))\n" [Pair (Symbol "a",Pair (Symbol "b", Pair (Pair (Symbol "c", Symbol "d"), Symbol "e")))];
  test_reader (* 63 *) "((a b c) . (d . ( (e . f) . g)))\n" [Pair (Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))),Pair (Symbol "d", Pair (Pair (Symbol "e", Symbol "f"), Symbol "g")))];
  test_reader (* 64 *) "(a b 1 (c . d) e . ())\n" [Pair (Symbol "a",Pair (Symbol "b",Pair (Number (Fraction (1, 1)),Pair (Pair (Symbol "c", Symbol "d"), Pair (Symbol "e", Nil)))))];
  test_reader (* 65 *) "(a b 1 (() . d) e . ())\n" [Pair (Symbol "a",Pair (Symbol "b",Pair (Number (Fraction (1, 1)),Pair (Pair (Nil, Symbol "d"), Pair (Symbol "e", Nil)))))];
  test_reader (* 66 *) "(().())\n" [Pair (Nil, Nil)];
  test_reader (* 67 *) "123456789e-9\n" [Number (Float 0.123456789)];
  test_reader (* 68 *) "1.23e+1\n" [Number (Float 12.3)];
  test_reader (* 72 *) "\"This is a very long\nstring that spills across\nseveral lines.\"\n" [String "This is a very long\nstring that spills across\nseveral lines."];
  test_reader (* 73 *) "2\n" [Number (Fraction (2, 1))];
  test_reader (* 74 *) "1.3\n" [Number (Float 1.3)];
  test_reader (* 75 *) "2.3\n" [Number (Float 2.3)];
  test_reader (* 76 *) "5.6\n" [Number (Float 5.6)];
  test_reader (* 77 *) "5\n" [Number (Fraction (5, 1))];
  test_reader (* 78 *) "()\n" [Nil];
  test_reader (* 79 *) "(    )\n" [Nil];
  test_reader (* 80 *) "7\n" [Number (Fraction (7, 1))];
  test_reader (* 81 *) "; sdfdker 4594359 fdskfs\n#t\n" [Bool true];
  test_reader (* 82 *) "#f;sadasujnxjzcn ij49\n" [Bool false];
  test_reader (* 83 *) ";asdi39isksdkmkdsf\n #t\n" [Bool true];
  test_reader (* 84 *) "#\\A\n" [Char 'A'];
  test_reader (* 85 *) "#\\d\n" [Char 'd'];
  test_reader (* 86 *) "#\\c\n" [Char 'c'];
  test_reader (* 87 *) "#\\k\n" [Char 'k'];
  test_reader (* 88 *) "aBc\n" [Symbol "abc"];
  test_reader (* 89 *) "ABC\n" [Symbol "abc"];
  test_reader (* 90 *) "DaG\n" [Symbol "dag"];
  test_reader (* 91 *) "dag\n" [Symbol "dag"];
  test_reader (* 92 *) "\"\\r\"\n" [String "\r"];
  test_reader (* 93 *) "\"\\n\"\n" [String "\n"];
  test_reader (* 94 *) "'(#\\P 2020.2 2020 \"COVID19\" . #t)\n" [Pair(Symbol("quote"),Pair(Pair(Char('P'), Pair(Number(Float(2020.2)), Pair(Number(Fraction (2020, 1)), Pair(String("COVID19"), Bool(true))))),Nil))];
  test_reader (* 95 *) "'(#\\O 101010.01 37392 \"comp\" #t)\n" [Pair(Symbol("quote"),Pair(Pair(Char('O'), Pair(Number(Float(101010.01)), Pair(Number(Fraction (37392, 1)), Pair(String("comp"), Pair(Bool(true), Nil))))),Nil))];
  test_reader (* 96 *) ",(#\\F 121212.212121 121212 \"the\" . #t)\n" [Pair(Symbol("unquote"),Pair(Pair(Char('F'), Pair(Number(Float(121212.212121)), Pair(Number(Fraction (121212, 1)), Pair(String("the"), Bool(true))))),Nil))];
  test_reader (* 97 *) ",(#\\D 23.32 12 \"okokok\" #t)\n" [Pair(Symbol("unquote"),Pair(Pair(Char('D'), Pair(Number(Float(23.32)), Pair(Number(Fraction (12, 1)), Pair(String("okokok"), Pair(Bool(true), Nil))))),Nil))];
  test_reader (* 98 *) ",!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456787\n" [Pair(Symbol("unquote"),Pair(Symbol("!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456787"),Nil))];
  test_reader (* 99 *) "`(#\\D 1234.1234 1234 \"OK\" . #T)\n" [Pair(Symbol("quasiquote"),Pair(Pair(Char('D'), Pair(Number(Float(1234.1234)), Pair(Number(Fraction (1234, 1)), Pair(String("OK"), Bool(true))))),Nil))];
  test_reader (* 100 *) "'10.99" [Pair(Symbol("quote"),Pair(Number(Float(10.99)),Nil))];
  test_reader (* 101 *) "'( 37392 )" [Pair(Symbol("quote"),Pair(Pair(Number(Fraction (37392, 1)),Nil),Nil))];
  test_reader (* 102 *) "'(#t . #f)" [Pair(Symbol("quote"),Pair(Pair(Bool(true),Bool(false)),Nil))];
  test_reader (* 103 *) "'()" [Pair(Symbol("quote"),Pair(Nil,Nil))];
  test_reader (* 104 *) ",\"This is a string with \\n \"" [Pair(Symbol("unquote"),Pair(String("This is a string with \n "),Nil))];
  test_reader (* 105 *) ",( #\\c . 303030 )" [Pair(Symbol("unquote"),Pair(Pair(Char('c'), Number(Fraction(303030, 1))),Nil))];
  test_reader (* 106 *) ",(#t . #f)" [Pair(Symbol("unquote"),Pair(Pair(Bool(true),Bool(false)),Nil))];
  test_reader (* 107 *) "`#\\newline" [Pair(Symbol("quasiquote"),Pair(Char('\n'),Nil))];
  test_reader (* 108 *) "`#\\tab" [Pair(Symbol("quasiquote"),Pair(Char('\t'),Nil))];
  test_reader (* 109 *) "`(#\\c 555.555 555.555 \"test\" . #f)" [Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(555.555)), Pair(Number(Float(555.555)), Pair(String("test"), Bool(false))))),Nil))];
  test_reader (* 110 *) ",@4" [Pair(Symbol("unquote-splicing"),Pair(Number(Fraction (4, 1)),Nil))];
  test_reader (* 111 *) ",@\"This is a string with unquote splicing \\n \"" [Pair(Symbol("unquote-splicing"),Pair(String("This is a string with unquote splicing \n "),Nil))];
  test_reader (* 112 *) ",@abcdefgh" [Pair(Symbol("unquote-splicing"),Pair(Symbol("abcdefgh"),Nil))];
  test_reader (* 113 *) ",@(#f . ababab)" [Pair(Symbol("unquote-splicing"),Pair(Pair(Bool(false),Symbol("ababab")),Nil))];
  test_reader (* 114 *) "#\\+" [Char '+'];
  test_reader (* 115 *) "#\\D" [Char 'D'];
  test_reader (* 116 *) "#\\K" [Char 'K'];
  test_reader (* 117 *) "( #\\a 555.555 )" [Pair(Char('a'), Pair(Number(Float(555.555)), Nil))];
  test_reader (* 118 *) "( \"test pair\" )" [Pair(String("test pair"), Nil)];
  test_reader (* 119 *) "9" [Number (Fraction (9, 1))];
  test_reader (* 120 *) "4" [Number (Fraction (4, 1))];
  test_reader (* 121 *) "(#\\newline #\\page #\\return #\\space #\\tab #\\newLINe #\\paGE #\\retURN #\\spACE #\\TaB)" [Pair (Char '\n', Pair (Char '\012', Pair (Char '\r', Pair (Char ' ', Pair (Char '\t', Pair (Char '\n', Pair (Char '\012', Pair (Char '\r', Pair (Char ' ', Pair (Char '\t', Nil))))))))))];
  test_reader (* 122 *) "(#\\A 123456.234 555 \"test\" . ;blablabla\n #t)" [Pair(Char('A'), Pair(Number(Float(123456.234)), Pair(Number(Fraction (555, 1)), Pair(String("test"), Bool(true)))))];
  test_reader (* 123 *) "1/7" [Number (Fraction (1, 7))];
  test_reader (* 124 *) "2/4" [Number (Fraction (1, 2))];
  test_reader (* 125 *) "-3/4" [Number (Fraction (-3, 4))];
  test_reader (* 126 *) "3/9" [Number (Fraction (1, 3))];
  test_reader (* 127 *) "18/21" [Number (Fraction (6, 7))];
  test_reader (* 128 *) "-30/300" [Number (Fraction (-1, 10))];
  test_reader (* 129 *) ",@(#T . 1/3)" [Pair(Symbol("unquote-splicing"),Pair(Pair(Bool(true),Number (Fraction (1, 3))),Nil))];
  test_reader (* 130 *) ",@\"String with unquote splicing\"" [Pair(Symbol("unquote-splicing"),Pair(String("String with unquote splicing"),Nil))];
  test_reader (* 131 *) ",@3/15" [Pair(Symbol("unquote-splicing"),Pair(Number(Fraction (1, 5)),Nil))];
  test_reader (* 132 *) ",@( #\\c 3/4 4/5 \"this\" )" [Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Pair(Number(Fraction (3, 4)), Pair(Number(Fraction (4, 5)), Pair(String("this"), Nil)))),Nil))];
  test_reader (* 133 *) "( #\\c 37392.39382 37392 ;fsdfds#$#$#%$#\n . \"that\" )\n" [Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Fraction(37392, 1)), String("that"))))];
  test_reader (* 134 *) "( #\\c 37392.39382 . 37393 )\n" [Pair(Char('c'), Pair(Number(Float(37392.39382)), Number(Fraction(37393, 1))))];;

let main () =
  Printf.printf "\nrunning tests...\n";
  our_tests ();
  official_tests ();;

main();;
