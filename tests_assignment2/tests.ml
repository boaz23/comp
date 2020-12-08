#use "tag-parser.ml";;

(* print types *)

let test_string_of_number = function
  | Fraction(num, denum) -> "Number(Fraction(" ^ (string_of_int num) ^ ", " ^ (string_of_int denum) ^ "))"
  | Float(float )-> "Number(Float(" ^ (string_of_float float) ^ "))";;


let rec test_string_of_sexpr_rec sexpr tab_acc =
  match sexpr with
  | Bool(bool) -> "Bool(" ^ (string_of_bool bool) ^ ")"
  | Nil -> "Nil"
  | Number(number) -> test_string_of_number number
  | Char(char) -> "Char(" ^ (String.make 1 char) ^ ")"
  | String(string) -> "String(" ^ string ^ ")"
  | Symbol(string) -> "Symbol(" ^ string ^ ")"
  | Pair(sexpr1, sexpr2) ->
        let tab_acc = tab_acc ^ "  " in
        let sexpr1_string = test_string_of_sexpr_rec sexpr1 tab_acc in
        let sexpr2_string = test_string_of_sexpr_rec sexpr2 tab_acc in
        "Pair (" ^ sexpr1_string ^ ", " ^ tab_acc ^ sexpr2_string ^ ")";;

let test_string_of_sexpr sexpr =
    test_string_of_sexpr_rec sexpr "\n";;

let rec sexpr_pair_fold_left f_list f_remainder acc sexpr =
    match sexpr with
    | Nil -> acc
    | Pair(car, cdr) -> (
        match cdr with
        | (Nil | Pair _) -> sexpr_pair_fold_left f_list f_remainder (f_list acc car) cdr
        | _ -> f_remainder (f_list acc car) cdr
    )
    | _ -> assert false

let rec test_string_of_sexpr_readable = function
  | Bool(true) -> "#t"
  | Bool(false) -> "#f"
  | Nil -> "'()"
  | Number(Fraction(n,d)) when d=1 -> string_of_int n
  | Number(Fraction(n,d)) -> (string_of_int n) ^ "/" ^ (string_of_int d)
  | Number(Float(f)) -> string_of_float f
  | Char(c) -> "#\\" ^ (String.make 1 c)
  | String(s) -> "\"" ^ s ^ "\""
  | Symbol(s) -> s
  | Pair(Symbol "quote", Pair(sexpr, Nil)) -> "'" ^ (test_string_of_sexpr_readable sexpr)
  | (Pair _ as pair) ->
    "(" ^ (
        let (string, _) =
          sexpr_pair_fold_left
            (fun (acc, is_first) sexpr ->
              let sexpr_string = test_string_of_sexpr_readable sexpr in
              let new_acc =
                if is_first then sexpr_string
                else acc ^ " " ^ sexpr_string in
              (new_acc, false))
            (fun (acc, is_first) sexpr ->
              let sexpr_string = test_string_of_sexpr_readable sexpr in
              let new_acc = acc ^ " . " ^ sexpr_string in
              (new_acc, is_first))
            ("", true)
            pair in
            string
    ) ^ ")"



let test_string_of_constant = function
  | Sexpr(sexpr) -> test_string_of_sexpr sexpr
  | Void -> "Void"

let rec test_string_of_expr_list expr_name expr_list =
    let expr_string_list = List.map test_string_of_expr expr_list in
    let exprs_string = String.concat "; " expr_string_list in
    expr_name ^ "([" ^  exprs_string ^ "])"

and test_string_of_2_exprs expr_name expr1 expr2 =
    let expr1_string = test_string_of_expr expr1 in
    let expr2_string = test_string_of_expr expr2 in
    expr_name ^ "(" ^  expr1_string ^ ", " ^ expr2_string ^ ")"

and test_string_of_lambda lambda_name string_list string_optional expr =
    let string_of_str_list = String.concat "; " string_list in
    let string_of_str_list = "[" ^ string_of_str_list ^ "]" in
    let expr_string = test_string_of_expr expr in
    let string_optional =
        if string_optional <> ""
        then ", " ^ string_optional
        else "" in
    lambda_name^ "(" ^ string_of_str_list ^ string_optional ^", " ^ expr_string ^ ")"

and test_string_of_expr = function
  | Const(constant) -> "Const(" ^ (test_string_of_constant constant) ^ ")"
  | Var(string) -> "Var(" ^ string ^ ")"
  | If(expr_if, expr_dit, expr_dif) ->
        let if_string = test_string_of_expr expr_if in
        let dit_string = test_string_of_expr expr_dit in
        let dif_string = test_string_of_expr expr_dif in
        "if(" ^ if_string ^ ", " ^ dit_string ^ ", " ^ dif_string ^ ")"
  | Seq(expr_list) -> test_string_of_expr_list "Seq" expr_list
  | Set(expr1, expr2) -> test_string_of_2_exprs "Set" expr1 expr2
  | Def(expr1, expr2) -> test_string_of_2_exprs "Def" expr1 expr2
  | Or(expr_list) -> test_string_of_expr_list "Or" expr_list
  | LambdaSimple(string_list, expr) -> test_string_of_lambda "LambdaSimple" string_list "" expr
  | LambdaOpt(string_list, string, expr) -> test_string_of_lambda "LambdaOpt" string_list string expr
  | Applic(expr, expr_list) ->
        let expr_string = test_string_of_expr expr in
        let expr_string_list = List.map test_string_of_expr expr_list in
        let exprs_string = "[" ^ (String.concat "; " expr_string_list) ^ "]" in
        "Applic(" ^ expr_string ^ ", " ^ exprs_string ^ ")";;



let rec compare_expr_equal_lenght_lists lst1 lst2 =
    if List.length lst1 = 0
        then true
        else
            let lst1_head = List.hd lst1 in
            let lst1_rest = List.tl lst1 in
            let lst2_head = List.hd lst2 in
            let lst2_rest = List.tl lst2 in
            if (expr_eq lst1_head lst2_head)
                then compare_expr_equal_lenght_lists lst1_rest lst2_rest
                else false;;

let compare_expr_list lst1 lst2 =
    if (List.length lst1 <> List.length lst2)
    then false
    else compare_expr_equal_lenght_lists lst1 lst2

let parse_and_compare sexpr excpected_expr =
    let excpected_expr_string = String.concat ";\n  " (List.map test_string_of_expr excpected_expr) in
    try(
    let expr_from_string =  Tag_Parser.tag_parse_expressions sexpr in
    let compare_result = compare_expr_list expr_from_string excpected_expr in
    let sexpr_string =  String.concat
                        "\n"
                        (List.map test_string_of_sexpr sexpr) in
    let result_expr_string = String.concat ";\n  " (List.map test_string_of_expr expr_from_string) in
    if compare_result = false
        then Printf.printf "error in test { %s }\n  expected: { %s }\n  actual: { %s }\n"
                            sexpr_string
                            excpected_expr_string
                            result_expr_string
    )
    with X_syntax_error -> Printf.printf "error Tag_Parser.X_syntax_error at %s" excpected_expr_string;;

let parse_and_compare_from_string string expr =
    try (
        let sexpr = Reader.read_sexprs string in
            parse_and_compare sexpr expr
    )
    with PC.X_no_match -> Printf.printf "PC.X_no_match at test %s\n" string;;

(* macro compare functions *)
let read_sexpr f string =
    try (
        let sexprs = Reader.read_sexprs string in
        match sexprs with
        | sexpr :: [] -> f sexpr
        | _ -> Printf.printf "Reader didn't return 1 sexpr %s\n" string
    )
    with PC.X_no_match -> Printf.printf "Reader got error at %s\n" string;;

let make_abstract_compare f_transformation f_output_to_string f_report_error sexpr expected =
    let sexpr_string = test_string_of_sexpr_readable sexpr in
    try (
        let actual = f_transformation sexpr in
        if (actual <> expected) then
            let actual_string = f_output_to_string actual in
            let expected_string = f_output_to_string expected in
                f_report_error sexpr_string actual_string expected_string
    )
    with X_syntax_error -> Printf.printf "error Tag_Parser.X_syntax_error at %s" sexpr_string;;

let make_compare_from_string f_compare string expected =
    read_sexpr
        (fun sexpr ->
            read_sexpr
                (fun expected_sexpr ->
                    f_compare
                        (fun sexpr_string actual_string expected_string ->
                            Printf.printf
                                "error in test { %s }\n  read: { %s }\n  expected: { %s }\n  actual: { %s }\n"
                                string
                                sexpr_string
                                expected_string
                                actual_string)
                        sexpr
                        expected_sexpr)
                expected)
        string;;

let make_compare_from_sexpr f_compare sexpr expected =
    f_compare
        (fun sexpr_string actual_string expected_string ->
            Printf.printf "  error in test { %s }\n  expected: { %s }\n  actual: { %s }\n"
                sexpr_string
                expected_string
                actual_string)
        sexpr
        expected;;

(*
let compare_tag_parse f_report_error sexpr expected =
    make_abstract_compare Tag_Parser.tag_parse test_string_of_expr f_report_error sexpr expected;;
let compare_tag_parse_from_string string expected =
    make_compare_from_string compare_tag_parse string expected;;
let compare_tag_parse_from_sexpr sexpr expected =
    make_compare_from_sexpr compare_tag_parse sexpr expected;;*)

let compare_macro_expansion expand_macro f_report_error sexpr expected =
    make_abstract_compare expand_macro test_string_of_sexpr_readable f_report_error sexpr expected;;
let compare_macro_expansion_from_sexpr expand_macro sexpr expected =
    make_compare_from_sexpr (compare_macro_expansion expand_macro) sexpr expected;;
let compare_macro_expansion_from_string expand_macro string expected =
    make_compare_from_string (compare_macro_expansion expand_macro) string expected;;
let compare_macro_expansion_from_string_shallow string expected_sexpr =
    compare_macro_expansion_from_string
        (fun sexpr ->
            let (expanded, _) = Tag_Parser.expand_macro_shallow sexpr in
                expanded)
        string expected_sexpr;;
let compare_macro_expansion_from_string_til_core_form string expected_sexpr =
    compare_macro_expansion_from_string Tag_Parser.expand_macro string expected_sexpr;;

let core_forms_tests() =

    (* consts *)
    parse_and_compare_from_string "1" [Const (Sexpr (Number (Fraction (1, 1))))];
    parse_and_compare_from_string "#t" [Const (Sexpr (Bool true))];
    parse_and_compare_from_string "#\\A" [Const (Sexpr (Char 'A'))];
    parse_and_compare_from_string "\"hello\"" [Const (Sexpr (String "hello"))];

    (* if statmenet *)
    parse_and_compare_from_string "(if #t #\\t)" [If (Const (Sexpr (Bool true)), Const (Sexpr (Char 't')), Const Void)];
    parse_and_compare_from_string "(if #t #\\t +16.2e1)"
                                    [If (Const (Sexpr (Bool true)), Const (Sexpr (Char 't')),
                                        Const (Sexpr (Number (Float 162.))))];

    (* applic and var *)
    parse_and_compare_from_string "(operator)" [Applic (Var "operator", [])];
    parse_and_compare_from_string "(operator operand1 operand2)" [Applic (Var "operator", [Var "operand1"; Var "operand2"])];
    parse_and_compare_from_string "(operator operand1 operand2 (if 1 2 3))"
                                    [Applic (Var "operator", [Var "operand1"; Var "operand2";
                                     If (Const (Sexpr (Number (Fraction (1, 1)))),
                                        Const (Sexpr (Number (Fraction (2, 1)))),
                                        Const (Sexpr (Number (Fraction (3, 1)))))])];

    (* or *)
    parse_and_compare_from_string "(or)" [Const(Sexpr(Bool(false)))];
    parse_and_compare_from_string "(or 1)" [Const(Sexpr(Number(Fraction(1, 1))))];
    parse_and_compare_from_string "(or operand1 2)" [Or [Var "operand1"; Const (Sexpr (Number (Fraction (2, 1))))]];

    (* set! *)
    parse_and_compare_from_string "(set! x 900)" [Set (Var "x", Const (Sexpr (Number (Fraction (900, 1)))))];
    parse_and_compare_from_string "(set! 1.ab (+ 1 2))"
                                    [Set (Var "1.ab",
                                     Applic (Var "+",
                                      [Const (Sexpr (Number (Fraction (1, 1))));
                                       Const (Sexpr (Number (Fraction (2, 1))))]))];
    (* parse_and_compare_from_string "(set! (fun 1 2) (+ 1 2))" *)

    (* sequance begin *)
    parse_and_compare_from_string "(begin)" [Const Void];
    parse_and_compare_from_string "(begin 6)" [Const (Sexpr (Number (Fraction (6, 1))))];
    parse_and_compare_from_string "(begin 1 2 (3 'hello))"
                                    [Seq
                                        [Const (Sexpr (Number (Fraction (1, 1))));
                                         Const (Sexpr (Number (Fraction (2, 1))));
                                          Applic (Const (Sexpr (Number (Fraction (3, 1)))),
                                          [Const (Sexpr (Symbol "hello"))])]];
    parse_and_compare_from_string "(begin 1 2 (begin 3 'hello) (begin) (begin \"a\") 'z)"
                                    [Seq
                                        [Const (Sexpr (Number (Fraction (1, 1))));
                                        Const (Sexpr (Number (Fraction (2, 1))));
                                        Const (Sexpr (Number (Fraction (3, 1)))); Const (Sexpr (Symbol "hello"));
                                        Const Void; Const (Sexpr (String "a")); Const (Sexpr (Symbol "z"))]];

    (* define *)
    parse_and_compare_from_string "(define var_name 'hello)" [Def (Var "var_name", Const (Sexpr (Symbol "hello")))];
    parse_and_compare_from_string "(define var (if 1 (begin 2 3) 2))"
                                    [Def (Var "var",
                                        If (Const (Sexpr (Number (Fraction (1, 1)))),
                                        Seq
                                            [Const (Sexpr (Number (Fraction (2, 1))));
                                            Const (Sexpr (Number (Fraction (3, 1))))],
                                        Const (Sexpr (Number (Fraction (2, 1))))))];

    (* lambda *)
    parse_and_compare_from_string "(lambda () 1)" [LambdaSimple ([], Const (Sexpr (Number (Fraction (1, 1)))))];

    parse_and_compare_from_string "(lambda () (if #f 4 (+ 1 2)) (begin))"
                                    [LambdaSimple ([], Seq
                                    [If (Const (Sexpr (Bool false)), Const (Sexpr (Number (Fraction (4, 1)))),
                                        Applic (Var "+",
                                        [Const (Sexpr (Number (Fraction (1, 1))));
                                        Const (Sexpr (Number (Fraction (2, 1))))]));
                                    Const Void])];

    parse_and_compare_from_string "(lambda (a x) (if a x (+ 1 2 x)) (begin x (* x 2)))"
                                    [LambdaSimple (["a"; "x"],
                                    Seq
                                    [If (Var "a", Var "x",
                                        Applic (Var "+",
                                        [Const (Sexpr (Number (Fraction (1, 1))));
                                         Const (Sexpr (Number (Fraction (2, 1)))); Var "x"]));
                                        Var "x";
                                    Applic (Var "*", [Var "x"; Const (Sexpr (Number (Fraction (2, 1))))])])];


    parse_and_compare_from_string "(lambda (a x . s) (if a x (apply function s)) (begin x (* x 2) s))"
                                    [LambdaOpt (["a"; "x"], "s", Seq
                                    [If (Var "a", Var "x", Applic (Var "apply", [Var "function"; Var "s"]));
                                        Var "x";
                                        Applic (Var "*", [Var "x"; Const (Sexpr (Number (Fraction (2, 1))))]);
                                        Var "s"])];


    parse_and_compare_from_string "(lambda c (if (eq? c '()) 1 2))"
                                    [LambdaOpt ([], "c", If (Applic (Var "eq?", [Var "c"; Const (Sexpr Nil)]),
                                        Const (Sexpr (Number (Fraction (1, 1)))),
                                        Const (Sexpr (Number (Fraction (2, 1))))))];

    parse_and_compare_from_string "(lambda c (begin (begin) (if (eq? c '()) 1 2)))"
                                    [LambdaOpt ([], "c", Seq
                                    [Const Void;
                                        If (Applic (Var "eq?", [Var "c"; Const (Sexpr Nil)]),
                                        Const (Sexpr (Number (Fraction (1, 1)))),
                                        Const (Sexpr (Number (Fraction (2, 1)))))])];

    parse_and_compare_from_string "(begin a (begin b (begin c (lambda () d (begin e f) g) h)))"
                                  [Seq[Var "a";
                                      Var "b";
                                      Var "c";
                                      LambdaSimple ([], Seq[Var "d"; Var "e";Var "f"; Var "g"]);
                                      Var "h"]];;

let macro_expansion_tests() =
    (* and *)
    compare_macro_expansion_from_string_shallow "(and)" "#t";
    compare_macro_expansion_from_string_shallow "(and 'hello)" "'hello";
    compare_macro_expansion_from_string_shallow "(and (lambda (a b) (+ a b)))"
        "(lambda (a b) (+ a b))";
    compare_macro_expansion_from_string_shallow "(and (or 1 2) (+ 1 2 1) \"str\")"
        "(if (or 1 2) (and (+ 1 2 1) \"str\") #f)";

    (* MIT define *)
    compare_macro_expansion_from_string_shallow "(define (add) (display \"hi\") 5)"
        "(define add (lambda () (display \"hi\") 5))";
    compare_macro_expansion_from_string_shallow "(define (add a b) (+ a b))"
        "(define add (lambda (a b) (+ a b)))";
    compare_macro_expansion_from_string_shallow "(define (function . a) a)"
        "(define function (lambda a a))";
    compare_macro_expansion_from_string_shallow "(define (function a b . c) (display (+ a b)) c)"
        "(define function (lambda (a b . c) (display (+ a b)) c))";
    compare_macro_expansion_from_string_shallow "(define (map list f) (display \"w\") (if (eq? list '()) list (cons (f (car list)) (map (cdr list) f))))"
        "(define map (lambda (list f) (display \"w\") (if (eq? list '()) list (cons (f (car list)) (map (cdr list) f)))))";

    (* let *)
    compare_macro_expansion_from_string_shallow "(let () 1)"
        "((lambda () 1))";
    compare_macro_expansion_from_string_shallow "(let ((a \"hello\\n\") (b 1)) (if a (display a) (display b)) (lambda () (* a b)))"
        "((lambda (a b)
            (if a (display a) (display b)) (lambda () (* a b)))
          \"hello\\n\" 1)";

    (* let* *)
    compare_macro_expansion_from_string_shallow "(let* () (display \"let* to let\"))"
        "(let () (display \"let* to let\"))";
    compare_macro_expansion_from_string_shallow "(let* ((a 'arg1)) (display \"let* to let\"))"
        "(let ((a 'arg1)) (display \"let* to let\"))";
    compare_macro_expansion_from_string_shallow "(let* ((a 'arg1) (b 'arg2)) (display \"let* to let with nested let*\"))"
        "(let ((a 'arg1)) (let* ((b 'arg2)) (display \"let* to let with nested let*\")))";

    (* letrec *)
    compare_macro_expansion_from_string_shallow "(letrec () 'letrec)"
        "(let () 'letrec)";
    compare_macro_expansion_from_string_shallow
        "(letrec ((function1 (lambda (n) (if (= n 0) n (+ n (function1 (- n 1)))))) (arg2 'arg2)) (display (function1 3)) arg2)"
        "(let ((function1 'whatever) (arg2 'whatever)) (set! function1 (lambda (n) (if (= n 0) n (+ n (function1 (- n 1)))))) (set! arg2 'arg2) (display (function1 3)) arg2)";

    (* pset! *)
    compare_macro_expansion_from_string_shallow "(pset! (x ((lambda (n) n) 1)))"
        "(set! x ((lambda (n) n) 1))";
    compare_macro_expansion_from_string_shallow "(pset! (x y) (y x))"
        "(let ((v_0 y) (v_1 x))
           (set! x v_0)
           (set! y v_1))";
    compare_macro_expansion_from_string_shallow "(pset! (x y) (y x) (x 1))"
        "(let ((v_0 y) (v_1 x) (v_2 1))
           (set! x v_0)
           (set! y v_1)
           (set! x v_2))";

    (* cond *)
    compare_macro_expansion_from_string_shallow "(cond ((= #t 1) 1))"
        "(if (= #t 1) (begin 1))";
    compare_macro_expansion_from_string_shallow "(cond (#t => 'hello))"
        "(let ((value #t) (f (lambda () 'hello))) (if value ((f) value)))";
    compare_macro_expansion_from_string_shallow
        "(cond ((lambda () 10) (+ 1 2) (display \"hello\") 1) (else (let* ((a 1) (b 2)) (+ (* a b) (if 1 1 1)))))"
        "(if (lambda () 10)
           (begin (+ 1 2) (display \"hello\") 1)
           (begin (let* ((a 1) (b 2)) (+ (* a b) (if 1 1 1)))))";

    compare_macro_expansion_from_string_shallow "(cond ((+ 1 2) 3) ('a => (eq? #t #t)) (else (begin 1 2 'hello)))"
        "(if (+ 1 2)
             (begin 3)
             (let ((value 'a)
                   (f (lambda () (eq? #t #t)))
                   (rest (lambda () (begin (begin 1 2 'hello)))))
                (if value
                    ((f) value)
                  (rest))))";

    compare_macro_expansion_from_string_shallow "(cond ('a => (eq? #t #t)) ('a1 => (list? cons(a b))))"
        "(let ((value 'a)
            (f (lambda () (eq? #t #t)))
            (rest (lambda ()
                    (let ((value 'a1)
                        (f (lambda () (list? cons(a b)))))
                      (if value ((f) value))))))
          (if value
              ((f) value)
            (rest)))";

     compare_macro_expansion_from_string_shallow "(cond ((+ 1 2) 3)('a => (eq? #t #t))(else (begin 1 2 'hello))(function (b 1) (a z)))"
        "(if (+ 1 2)
           (begin 3)
           (let ((value 'a)
                 (f (lambda () (eq? #t #t)))
                 (rest (lambda () (begin (begin 1 2 'hello)))))
             (if value
               ((f) value)
               (rest))))";

     compare_macro_expansion_from_string_shallow
        "(cond ((zero? n) (f x) (g y))
               ((h? x) => (p q))
               (else (h x y) (g x))
               ((q? y) (p x) (q y)))"
        "(if (zero? n)
             (begin (f x) (g y))
             (let ((value (h? x))
                   (f (lambda () (p q)))
                   (rest (lambda () (begin (h x y) (g x)))))
               (if value
                   ((f) value)
                 (rest))))";


    compare_macro_expansion_from_string_shallow "`,x" "x";
    (* compare_macro_expansion_from_string_shallow "`,@x" ""; *)
    compare_macro_expansion_from_string_shallow "`(a b)" "(cons 'a (cons 'b '()))";
    compare_macro_expansion_from_string_shallow "`(,a b)" " (cons a (cons 'b '()))";
    compare_macro_expansion_from_string_shallow "`(a ,b)" " (cons 'a (cons b '()))";
    compare_macro_expansion_from_string_shallow "`(,@a b) " "(append a (cons 'b '()))";
    compare_macro_expansion_from_string_shallow "`(a ,@b)" "(cons 'a (append b '()))";
    compare_macro_expansion_from_string_shallow "`(,a ,@b)" "(cons a (append b '()))";
    compare_macro_expansion_from_string_shallow "`(,@a ,@b) " "    (append a (append b '()))";
    compare_macro_expansion_from_string_shallow "`(,@a . ,b)" "(append a b)";
    compare_macro_expansion_from_string_shallow "`(,a . ,b) " "(cons a b)";
    compare_macro_expansion_from_string_shallow "`(((,@a)))" " (cons (cons (append a '()) '()) '())";
    compare_macro_expansion_from_string_shallow "`(,a . ,@b)" "(cons a '(unquote-splicing b))";
    compare_macro_expansion_from_string_shallow " `(list ,@(map describe e))"
        "(cons 'list (append (map describe e) '()))";
    compare_macro_expansion_from_string_shallow "`(cons ,(describe (car e)) ,(describe (cdr e)))"
        "(cons 'cons (cons (describe (car e)) (cons (describe (cdr e)) '())))";
    compare_macro_expansion_from_string_shallow "`(vector ,@(map describe (vector->list e)))"
        "(cons 'vector (append (map describe (vector->list e)) '()))";
    compare_macro_expansion_from_string_shallow "`',e" "(cons 'quote (cons e '()))";;

let genaral_tests() =

    parse_and_compare_from_string "(pset! (x ((lambda (n) n) 1)))"
        [Set (
            Var "x",
            Applic (
                LambdaSimple (["n"], (Var "n")),
                [Const (Sexpr (Number (Fraction(1, 1))))]
            )
        )];

    parse_and_compare_from_string
        "(let* ((a 'arg1)
                (b 'arg2))
            (display \"let* to let with nested let*\")
            (and a b))"
        [Applic(LambdaSimple(["a"],
                Applic(LambdaSimple(["b"],
                        Seq([
                            Applic(Var("display"),
                                   [Const(Sexpr(String "let* to let with nested let*"))]);
                            If(Var("a"), Var("b"), Const(Sexpr(Bool(false))))
                        ])),
                [Const(Sexpr(Symbol("arg2")))])),
            [Const(Sexpr(Symbol("arg1")))])];

    parse_and_compare_from_string
        "(define var1 1)
         (define var2 'hello)
         (pset! (var1 var2) (var2 var1))
        "
        [Def(Var("var1"), Const(Sexpr(Number(Fraction(1, 1)))));
         Def(Var("var2"), Const(Sexpr(Symbol("hello"))));
         Applic(LambdaSimple(["v_0"; "v_1"],
                Seq[
                    Set(Var("var1"), Var("v_0"));
                    Set(Var("var2"), Var("v_1"))
                ]),
            [Var("var2"); Var("var1")])];

    parse_and_compare_from_string
        "(letrec (
            (function (lambda (a . c)
                (cond ((empty? c) => a)
                        (else (display a) (length c))))))
            (function 10 1 2 3)
        )"

        [Applic(LambdaSimple(["function"],
                Seq([
                    Set(Var("function"),
                        LambdaOpt(["a"], "c",
                            Applic(LambdaSimple(["value"; "f"; "rest"],
                                If(Var("value"),
                                    Applic(Applic(Var("f"), []), [Var("value")]),
                                    Applic(Var("rest"), []))
                                )
                            , [
                                Applic(Var("empty?"), [Var("c")]);
                                LambdaSimple([], Var("a"));
                                LambdaSimple([], Seq([
                                    Applic(Var("display"), [Var("a")]);
                                    Applic(Var("length"), [Var("c")])
                                ]))
                            ]))
                        );
                    Applic(Var("function"),
                        [
                            Const(Sexpr(Number(Fraction(10, 1))));
                            Const(Sexpr(Number(Fraction(1, 1))));
                            Const(Sexpr(Number(Fraction(2, 1))));
                            Const(Sexpr(Number(Fraction(3, 1))))
                    ])
                ])),
            [Const(Sexpr(Symbol("whatever")))])];

    parse_and_compare_from_string
        "(define (add) (display \"hi\") 5)"
        [Def(Var("add"),
            LambdaSimple([],
            Seq([
                 Applic(Var("display"), [Const(Sexpr(String("hi")))]);
                 Const(Sexpr(Number(Fraction(5, 1))))
            ]))
        )];

    parse_and_compare_from_string
        "(define (add a b) (+ a b))"
        [Def(Var("add"),
            LambdaSimple(["a"; "b"],
                Applic(Var("+"), [Var("a"); Var("b")])
            )
        )];

    parse_and_compare_from_string
        "(define (function . a) a)"
        [Def(Var("function"),
            LambdaOpt([], "a",
                Var("a"))
        )];

    parse_and_compare_from_string
        "(define (function a b . c) (display (+ a b)) c)"
        [Def(Var("function"),
            LambdaOpt(["a"; "b"], "c",
            Seq([
                 Applic(Var("display"), [Applic(Var("+"), [Var("a"); Var("b")])]);
                 Var("c")
            ]))
        )];;

let main() =
    begin
        core_forms_tests();
        macro_expansion_tests();
        genaral_tests()
    end;;