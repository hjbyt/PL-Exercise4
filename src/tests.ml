(*
  Tests for the lambda calculus parser and reducers.

  EXTEND THIS FILE TO TEST YOUR SOLUTION THOROUGHLY!
*)

open Utils
open Parser
open Reducer

let rec evaluate ~verbose reduce t =
  if verbose then print_string (format_term t) else ();
  match reduce t with
  | None ->
    if verbose then print_string " =/=>\n\n" else ();
    t
  | Some t' ->
    if verbose then print_string " ==>\n\n" else ();
    evaluate ~verbose reduce t'


let test_and_1 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
"

let test_and_2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) ((and tru) tru))
"

let env = "
let tru = (\\t. (\\f. t)) in
let fls = (\\t. (\\f. f)) in
let test = (\\l. (\\m. (\\n. ((l m) n)))) in
let and = (\\b. (\\c.  ((b c) fls))) in

let pair = (\\f. (\\s. (\\b.  ((b f) s)))) in
let fst = (\\p. (p tru)) in
let snd = (\\p. (p fls)) in

let c0 = (\\s. (\\z. z)) in
let c1 = (\\s. (\\z. (s z))) in
let c2 = (\\s. (\\z. (s (s z)))) in
let c3 = (\\s. (\\z. (s (s (s z))))) in
let c4 = (\\s. (\\z. (s (s (s (s z)))))) in
let c5 = (\\s. (\\z. (s (s (s (s (s z))))))) in
let c6 = (\\s. (\\z. (s (s (s (s (s (s z)))))))) in
let c7 = (\\s. (\\z. (s (s (s (s (s (s (s z))))))))) in
let c8 = (\\s. (\\z. (s (s (s (s (s (s (s (s z)))))))))) in
let c9 = (\\s. (\\z. (s (s (s (s (s (s (s (s (s z))))))))))) in
let c10 = (\\s. (\\z. (s (s (s (s (s (s (s (s (s (s z)))))))))))) in

let scc = (\\n. (\\s. (\\z. (s ((n s) z))))) in
let plus = (\\m. (\\n. (\\s. (\\z. ((m s) ((n s) z)))))) in
let times = (\\m. (\\n. (\\s. (m (n s))))) in
let power = (\\m. (\\n. (n m))) in
let iszero = (\\m. ((m (\\x. fls)) tru)) in
let prd = (let zz = ((pair c0) c0) in
           let ss = (\\p. ((pair (snd p)) ((plus c1) (snd p)))) in
           (\\m. (fst ((m ss) zz)))) in
let leq = (\\m. (\\n. (iszero ((n prd) m)))) in
let equal = (\\m. (\\n. ((and ((leq m) n)) ((leq n) m)))) in

let Y = (\\f. ((\\x. (f (x x))) (\\x. (f (x x))))) in
let Z = (\\f. ((\\x. (f (\\y. ((x x) y)))) (\\x. (f (\\y. ((x x) y)))))) in
"

let test_fact_l = env ^ "
let fact_l = (Y (\\f. (\\n. (((test (iszero n)) c1) (((times n) (f (prd n)))))))) in
((equal (fact_l c2)) c2)
"

let test_fact_s = env ^ "
let fact_s = (Z (\\f. (\\n. ((((test (iszero n)) (\\x. c1)) (\\x. (((times n) (f (prd n)))))) (\\x.x))))) in
((equal (fact_s c2)) c2)
"

(* Extra tests *)

let test_fact_l2 = env ^ "
let fact_l = (Y (\\f. (\\n. (((test (iszero n)) c1) (((times n) (f (prd n)))))))) in
((equal (fact_l c3)) c6)
"

let test_fact_s2 = env ^ "
let fact_s = (Z (\\f. (\\n. ((((test (iszero n)) (\\x. c1)) (\\x. (((times n) (f (prd n)))))) (\\x.x))))) in
((equal (fact_s c3)) c6)
"

let parse_conv_test_cases = [
    ("x", "x");
    ("(x)", "x");
    ("x y", "(x y)");
    ("\\x. y", "(\\x. y)");
    ("\\x. \\y. \\z. x y z", "(\\x. (\\y. (\\z. ((x y) z))))");
    ("a b c d e f", "(((((a b) c) d) e) f)");
    ("a b (c d) e f", "((((a b) (c d)) e) f)");
    ("let x = \\y. a (b c) d in x f", "let x = (\\y. ((a (b c)) d)) in (x f)");
]

let check_parse_conv (conv_string, strict_string) = 
    let parsed_conv = parse_conv conv_string in
    let parsed_string = parse strict_string in
    let formatted_conv = format_term parsed_conv in
    let formatted_string = format_term parsed_string in
    let prefix = 
        if formatted_conv = formatted_string
        then "OK: "
        else "MISSMATCH: \"" ^ formatted_conv ^ "\" != \"" ^ formatted_string ^ "\" --- "
    in
    print_string (prefix ^ "\"" ^ conv_string ^ "\", \"" ^ strict_string ^ "\"\n")

let check_all_parse_conv_cases () = List.iter check_parse_conv parse_conv_test_cases

let format_term_conv_cases = [
    (parse_conv "x", "x");
    (parse_conv "(x)", "x");
    (parse_conv "x y", "x y");
    (parse_conv "(x y)", "x y");
    (parse_conv "a b c d e", "a b c d e");
    (parse_conv "a b (c d) e", "a b (c d) e");
    (parse_conv "\\x. \\y. \\z. x y z", "\\x. \\y. \\z. x y z");
    (parse_conv "(\\x. y) t", "(\\x. y) t");
]

let check_format_term_conv (term, expected) =
    let formatted = format_term_conv term in
    let output = 
        if formatted = expected
        then "OK: \"" ^ formatted ^ "\" = \"" ^ expected ^ "\""
        else "MISSMATCH: \"" ^ formatted ^ "\" != \"" ^ expected ^ "\""
    in
    print_string (output ^ "\n")
    
let check_all_format_term_conv_cases () = List.iter check_format_term_conv format_term_conv_cases

(* /Extra tests *)

let test ~verbose ~sem ~reduce s =
  printf "\nEvaluating:\n%s\nin %s semantics:\n\n" s sem;
  let result = (evaluate ~verbose reduce (parse s)) in
  printf "Result is: %s\n\n" (format_term result)

let test_lazy = test ~sem:"lazy" ~reduce:reduce_lazy
let test_strict = test ~sem:"strict" ~reduce:reduce_strict
let test_normal = test ~sem:"normal-order" ~reduce:reduce_normal
let test_all ~verbose s =
  test_lazy ~verbose s;
  test_strict ~verbose s;
  test_normal ~verbose s


let () =
  test_all ~verbose:true test_and_1;
  test_all ~verbose:true test_and_2;

  test_lazy ~verbose:false test_fact_l;
  test_strict ~verbose:false test_fact_s;
  test_normal ~verbose:false test_fact_l;
  test_normal ~verbose:false test_fact_s;
  
  (* Extra tests *)
  
  test_lazy ~verbose:false test_fact_l2;
  test_strict ~verbose:false test_fact_s2;
  test_normal ~verbose:false test_fact_l2;
  test_normal ~verbose:false test_fact_s2;
  
  check_all_parse_conv_cases ();
  print_string "\n";
  check_all_format_term_conv_cases ();

