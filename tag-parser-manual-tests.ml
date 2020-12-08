#use "tag-parser.ml";;

let rec describe_sexpr = function
  | Bool(true) -> "#t"
  | Bool(false) -> "#f"
  | Nil -> "'()"
  | Number(Fraction(n,d)) when d=1 -> string_of_int n
  | Number(Fraction(n,d)) -> Printf.sprintf "%d/%d" n d
  | Number(Float(f)) -> string_of_float f
  | Char(c) -> Printf.sprintf "#\\%c" c
  | String(s) -> Printf.sprintf "\"%s\"" s
  | Symbol(s) -> Printf.sprintf "'%s" s
  | Pair(x, y) -> Printf.sprintf "(cons %s %s)" (describe_sexpr x) (describe_sexpr y);;

let test_string_tag_parse str =
  let sexpr = Reader.read_sexprs str in
    Tag_Parser.tag_parse_expressions sexpr;;

let test_expand_macro_deep sexpr =
  let rec expand sexpr is_expanded =
    (let (sexpr, has_been_expanded) = Tag_Parser.expand_macro_shallow sexpr in
      if has_been_expanded then (expand sexpr true)
      else (sexpr, is_expanded)) in
    expand sexpr false;;

let test_string_macro_expand_custom str expand_macro =
  let sexprs = Reader.read_sexprs str in
    match sexprs with
    | sexpr :: [] ->
        let (sexpr, has_been_expanded) = expand_macro sexpr in
        let expansion_status =
          if has_been_expanded then "expanded"
          else "unchanged" in
          (expansion_status, sexpr)
    | _ -> assert false;;
let test_string_macro_expand str = test_string_macro_expand_custom str Tag_Parser.expand_macro_shallow;;
let test_string_macro_expand_deep str = test_string_macro_expand_custom str test_expand_macro_deep;;

let describe_string_macro_expand_custom str expand_macro =
  let (expansion_status, sexpr) = test_string_macro_expand_custom str expand_macro in
    Printf.printf "%s%s" (expansion_status^"\n") (describe_sexpr sexpr);;
let describe_string_macro_expand str = describe_string_macro_expand_custom str Tag_Parser.expand_macro_shallow;;
let describe_string_macro_expand_deep str = describe_string_macro_expand_custom str test_expand_macro_deep;;