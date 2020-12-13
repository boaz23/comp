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
let make_delimited_on_left nt_delimiter nt = pack (caten nt_delimiter nt) (fun (_, value) -> value);;

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
let nt_boolean_greedy_take =
  let nt_boolean_values = make_special_token_list [('f', false); ('t', true)] char_ci in
  let parser = make_delimited_on_left nt_char_hashtag nt_boolean_values in
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
let nt_char_digit_list = plus nt_char_digit;;

let nt_char_sign_raw = disj (char '+') (char '-');;
let nt_optional_char_sign = maybe nt_char_sign_raw;;
let nt_char_sign = pack_option nt_optional_char_sign '+';;

let make_raw_int nt_int_string = pack nt_int_string (fun s -> int_of_string s);;

let nt_natural_string = pack nt_char_digit_list (fun s -> list_to_string s);;
let nt_natural = make_raw_int nt_natural_string;;

let nt_signed_integer_string =
  let packer = fun (s, n) -> list_to_string (s :: n) in
  pack (caten nt_char_sign nt_char_digit_list) packer;;
let nt_int_raw = make_raw_int nt_signed_integer_string;;

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

let make_float_from_string nt_float_raw_string = pack nt_float_raw_string (fun f_s -> Float (float_of_string f_s));;
let nt_float_point_string =
  let nt_char_frac_delim = char '.' in
  let mantissa_whole = nt_signed_integer_string in
  let mantisaa_frac = nt_natural_string in
  let parser = make_delimited_pair mantissa_whole mantisaa_frac nt_char_frac_delim in
  let packer = (fun (m_int, m_frac) -> Printf.sprintf "%s.%s" m_int m_frac) in
  pack parser packer;;

let nt_float_point = make_float_from_string nt_float_point_string;;
let nt_float_scientific_string =
  let nt_char_ci_exp = char_ci 'e' in
  let nt_base = disj nt_float_point_string nt_signed_integer_string in
  let parser = make_delimited_pair nt_base nt_signed_integer_string nt_char_ci_exp in
  let pack_float_scientific = fun (base, exp) -> Printf.sprintf "%se%s" base exp in
  pack parser pack_float_scientific;;
let nt_float_scientific = make_float_from_string nt_float_scientific_string;;

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
  let parser_string_meta_char_inner =
    let string_meta_char_list = [
      ('\\', '\\');
      ('"', '"');
      ('t', '\t');
      ('f', '\012');
      ('n', '\n');
      ('r', '\r')
    ] in
    make_special_token_list string_meta_char_list char_ci in
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
and nt_quotes s =
  let nt_quote_delimiters =
    let quote_tokens_list = [
      ("'", "quote");
      ("`", "quasiquote");
      (",@", "unquote-splicing");
      (",", "unquote")
    ] in
    make_special_token_list quote_tokens_list word in
  let nt_quoted_sexpr = caten nt_quote_delimiters nt_sexpr in
  let quote_packer = (fun (name, sexpr) -> Pair(Symbol(name), Pair(sexpr, Nil))) in
  let parser = pack nt_quoted_sexpr quote_packer in
  parser s

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
and nt_sexpr s =
  let nt_sexpr_core =
    let sexpr_parsers_list = [
      nt_boolean; nt_char; nt_number; nt_string;
      nt_symbol; nt_pairs; nt_quotes
    ] in
    disj_list sexpr_parsers_list in
  let parser = make_paired_sym nt_invisible nt_sexpr_core in
  parser s;;

let nt_sexprs_input = make_paired nt_invisible nt_end_of_input (star nt_sexpr);;

let read_sexprs string =
  let s = string_to_list string in
  let (sexprs, _) = nt_sexprs_input s in
  sexprs;;

end;; (* struct Reader *)
