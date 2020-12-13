#use "pc.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

type number =
  | Fraction of int * int
  | Float of float;;

type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

module Reader: sig
  val read_sexprs : string -> sexpr list
end
= struct
open PC;;

let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;

(*
------------ literal characters parsers ------------
*)
let nt_char_digit = (range '0' '9');;
let nt_char_backslash = char '\\';;
let nt_char_double_quote = char '"';;
let nt_char_hashtag = char '#';;
let nt_char_open_parentheses = char '(';;
let nt_char_close_parentheses = char ')';;
let nt_lowercase = range 'a' 'z';;
let nt_uppercase = range 'A' 'Z';;
let nt_new_line_feed = char '\n';;
let nt_dot = char '.';;

(*
------------ cross-cutting & utility functions ------------
*)
let list_to_lowercase_string s =
  String.concat "" (List.map (fun ch -> String.make 1 (lowercase_ascii ch)) s);;

let make_ignored nt = pack nt (fun _ -> ());;

let make_special_token_list base_list nt_token =
  let nt_special_list = List.map (fun (special_token, token_value) ->
                                    pack (nt_token special_token)
                                         (fun _ -> token_value))
                                 base_list in
  disj_list nt_special_list;;

(* ----- Option ----- *)
let option_value_or_default default = function
  | Some value -> value
  | None -> default;;
let pack_option nt_opt default =
  pack nt_opt (option_value_or_default default);;

(* ----- Delimiters ----- *)
let make_paired nt_left nt_right nt =
    let nt = caten nt_left nt in
    let nt = pack nt (function(_, e) -> e) in
    let nt = caten nt nt_right in
    let nt = pack nt (function(e, _) -> e) in
    nt;;
let make_paired_sym nt_sides nt = make_paired nt_sides nt_sides nt;;
let make_delimited_pair nt_left nt_right nt_delim =
  pack (caten (caten nt_left nt_delim) nt_right)
       (fun ((left, _), right) -> (left, right));;
let make_delimited_on_left nt_left nt = pack (caten nt_left nt) (fun (_, value) -> value);;

(*
------------ whitespaces ------------
*)
let nt_whitespaces = star nt_whitespace;;
let make_spaced nt = make_paired_sym nt_whitespaces nt;;

let nt_line_comment =
  let nt_dot_wildcard = diff nt_any nt_new_line_feed in
  let comment_prefix = char ';' in
  let comment_end = disj (make_ignored nt_end_of_input) (make_ignored nt_new_line_feed) in
  make_paired comment_prefix comment_end (star nt_dot_wildcard)

(*
------------ symbols ------------
*)
let nt_symbol_char_no_dot =
  let punctuation_char_list = string_to_list "!$^*-_+=<>/?:" in
  let nt_punctuation_list = List.map char punctuation_char_list in
  let nt_chars_alpha_numeric = [nt_char_digit; nt_lowercase; nt_uppercase] in
  let nt_symbol_char_no_dot_list = nt_chars_alpha_numeric @ nt_punctuation_list in
    disj_list nt_symbol_char_no_dot_list;;

let nt_symbol_char = disj nt_symbol_char_no_dot nt_dot;;

let nt_symbol =
  let nt_noDot_with_starOfSymbolChar = caten nt_symbol_char_no_dot (star nt_symbol_char) in
  let nt_dot_with_plusOfSymbolChar = caten nt_dot (plus nt_symbol_char) in

  let parser = disj nt_noDot_with_starOfSymbolChar nt_dot_with_plusOfSymbolChar in
  let packer = fun (e, es) -> Symbol(list_to_lowercase_string (e :: es)) in
    pack parser packer;;

(*
------------ boolean ------------
*)
let nt_ci_f = char_ci 'f';;
let nt_ci_t = char_ci 't';;

let nt_boolean_greedy_take =
  let parser_false = pack nt_ci_f (fun _ -> false) in
  let parser_true  = pack nt_ci_t (fun _ -> true)  in
  let parser_inner = disj parser_false parser_true in
  let parser = make_delimited_on_left nt_char_hashtag parser_inner in
    pack parser (fun b -> Bool(b));;

let nt_boolean = not_followed_by nt_boolean_greedy_take nt_symbol_char;;

(*
------------ characters ------------
*)
let nt_visible_simple_char = diff nt_any nt_whitespace;;
let nt_named_char =
  let named_chars_list = [
    ("nul", '\000');
    ("newline", '\n');
    ("page", '\012');
    ("return", '\r');
    ("space", ' ');
    ("tab", '\t')
  ] in
  make_special_token_list named_chars_list word_ci;;

let nt_char_char = disj nt_named_char nt_visible_simple_char;;
let nt_char_greedy_take =
  let nt_sexpr_char_prefix = caten nt_char_hashtag nt_char_backslash in
  let parser = make_delimited_on_left nt_sexpr_char_prefix nt_char_char in
    pack parser (fun ch -> Char(ch));;

let nt_char = not_followed_by nt_char_greedy_take nt_symbol_char;;

(*
------------ numbers ------------

TODO: fix backtracking
*)
let nt_num_digit =
  let ascii_0 = int_of_char '0' in
    pack nt_char_digit (fun ch -> (int_of_char ch) - ascii_0);;
let nt_num_digit_list = plus nt_num_digit;;

let nt_optional_sign =
  let nt_char_sign_plus = char '+' in
  let nt_char_sign_minus = char '-' in
  let parser_sign_plus = pack nt_char_sign_plus (fun _ -> 1) in
  let parser_sign_minus = pack nt_char_sign_minus (fun _ -> -1) in
  let nt_num_sign = disj parser_sign_plus parser_sign_minus in
    maybe nt_num_sign;;

let nt_num_sign = pack_option nt_optional_sign 1;;

let nt_natural =
  let f = fun a b -> a * 10 + b in
  let packer = fun s -> List.fold_left f 0 s in
    pack nt_num_digit_list packer;;

let nt_int_with_sign_pair =
  let packer = fun (s, n) -> (n, s) in
  pack (caten nt_num_sign nt_natural) packer;;
let nt_int_raw = pack nt_int_with_sign_pair (fun (n, s) -> n * s);;

let nt_integer = pack nt_int_raw (fun n -> Fraction(n, 1));;

let nt_fraction =
  let nt_char_op_div = char '/' in
  let parser = make_delimited_pair nt_int_raw nt_natural nt_char_op_div in
  let packer =
    let rec gcd n m =
      if m = 0 then n
      else gcd m (n mod m) in
    fun (nom, dom) ->
      let gcd = gcd (abs nom) dom in
        Fraction(nom / gcd, dom / gcd) in
    pack parser packer;;

let make_float nt_float_raw = pack nt_float_raw (fun f -> Float(f));;

let nt_float_frac =
  let f = fun a b -> (float_of_int a +. b) /. 10.0 in
  let packer = fun s -> List.fold_right f s 0.0 in
    pack nt_num_digit_list packer;;

let nt_float_point_raw =
  let nt_char_frac_delim = char '.' in
  let parser = make_delimited_pair nt_int_with_sign_pair nt_float_frac nt_char_frac_delim in
  let packer =
    fun ((n, s), frac) ->
      let f = (float_of_int n) in
      let f = f +. frac in
      if s = 1 then f
      else -.f in
    pack parser packer;;

let nt_float_point = make_float nt_float_point_raw;;

let nt_float_scientific_raw =
  let nt_char_ci_exp = char_ci 'e' in
  let mantisaa_part = disj nt_float_point nt_integer in
  let parser = make_delimited_pair mantisaa_part nt_int_raw nt_char_ci_exp in
  let pack_float_scientific =
    fun (base, exp) ->
      let e = float_of_int exp in
      let b = (
        match base with
          | Fraction(n, m) -> (float_of_int n) /. (float_of_int m)
          | Float(f) -> f) in
        b *. (10.0 ** e) in
    pack parser pack_float_scientific;;
let nt_float_scientific = make_float nt_float_scientific_raw;;

let nt_number_greedy_take =
  let parsers_list = [
    nt_float_scientific;
    nt_float_point;
    nt_fraction;
    nt_integer
  ] in
  pack (disj_list parsers_list) (fun num -> Number(num));;

let nt_number = not_followed_by nt_number_greedy_take (diff nt_symbol_char nt_char_digit);;

(*
------------ string ------------
*)
let nt_string_literal_char = diff nt_any (disj (char '\\') (char '"'));;
let nt_string_meta_char =
  let string_meta_char_list = [
    ('\\', '\\');
    ('"', '"');
    ('t', '\t');
    ('f', '\012');
    ('n', '\n');
    ('r', '\r')
  ] in
  let parser_string_meta_char_inner = make_special_token_list string_meta_char_list char_ci in
  make_delimited_on_left nt_char_backslash parser_string_meta_char_inner


let nt_string_char = disj nt_string_literal_char nt_string_meta_char;;
let nt_string =
  let parser = make_paired_sym nt_char_double_quote (star nt_string_char) in
  let packer = fun s -> String(list_to_string s) in
    pack parser packer;;

(*
------------ recursive sepxr production rules ------------
*)
(* ----- list & dotted list ----- *)
let rec nt_pairs s =
  let nt_opening_parenthesis = caten nt_char_open_parentheses nt_invisible in
  let nt_sexprs_opt =
    let nt_sexprs =
      let nt_unpacked_sexprs =
        let nt_inner_sexprs = plus nt_sexpr in
        let nt_last_sexpr =
          let nt_dotted_sexpr = make_delimited_on_left nt_dot nt_sexpr in
          pack_option (maybe nt_dotted_sexpr) Nil in
        caten nt_inner_sexprs nt_last_sexpr in
      let pack_sexprs (sexprs_list, last_sexpr) =
        List.fold_right (fun e acc -> Pair(e, acc)) sexprs_list last_sexpr in
      pack nt_unpacked_sexprs pack_sexprs in
    pack_option (maybe nt_sexprs) Nil in
  let parser = make_paired nt_opening_parenthesis nt_char_close_parentheses nt_sexprs_opt in
  parser s

(* ----- quotes expressions ----- *)
(* No need to explicitly enclose the quote in invisible stuff
   because the sexpr handles it already *)
and make_quoted nt_quote name s = pack (make_delimited_on_left nt_quote nt_sexpr)
                                       (fun sexpr -> Pair(Symbol(name), Pair(sexpr, Nil)))
                                       s
and nt_quoted s = make_quoted (word "\'") "quote" s
and nt_quasi_quoted s = make_quoted (word "`") "quasiquote" s
and nt_unquoted s = make_quoted (not_followed_by (word ",") (char '@')) "unquote" s
and nt_unquote_and_spliced s = make_quoted (word ",@") "unquote-splicing" s

(* ----- invisible stuff handling (whitespaces and comments) ----- *)
and nt_sexpr_comment s = (caten (word "#;") nt_sexpr) s
and nt_invisible s =
  let invisible_parser = disj_list [
    (make_ignored (plus nt_whitespace)); (* to make it a clear distiction when the whitespaces parser
                                            actually trimms whitspaces and when it does not.
                                            this is crutial so that the disjunction may actually fail *)
    (make_ignored nt_line_comment);
    (make_ignored nt_sexpr_comment)
  ] in
    star invisible_parser s

(* ----- sexpr ----- *)
and parser_nts_list = [nt_boolean; nt_char; nt_number; nt_string;
                       nt_symbol; nt_pairs; nt_quoted;
                       nt_quasi_quoted; nt_unquoted; nt_unquote_and_spliced]
and nt_sexpr s = make_paired_sym nt_invisible (disj_list parser_nts_list) s;;

let nt_sexprs_input = make_paired nt_invisible nt_end_of_input (star nt_sexpr);;

let read_sexprs string =
  let s = string_to_list string in
  let (sexprs, _) = nt_sexprs_input s in
    sexprs;;

end;; (* struct Reader *)
