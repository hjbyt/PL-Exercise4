(*
  Parser for lambda-calculus.
*)

open Utils
open Lexer


(* AST for lambda expressions - DO NOT MODIFY THIS TYPE *)
type term = | Variable of string
	    | Abstraction of string * term
	    | Application of term * term

(*
  Concrete Syntax:
  t ::= id | (\id.t) | (t1 t2) | (t) | let id=t1 in t2

  Abstract Syntax:
  term ::= id | \id.term | term1 term2
*)

exception SyntaxError of string

(*
  ADD FUNCTIONS BELOW
*)

let rec parse_term = function
    (* Variable *)
    | Literal id :: tokens' -> (Variable id, tokens')
    (* Let expression *)
    | LetTok :: Literal id :: EqTok :: tokens' ->
        let t1, tokens'' = parse_term tokens' in (
        match tokens'' with
        | InTok :: tokens''' ->
            let t2, tokens'''' = parse_term tokens''' in
            (Application ((Abstraction (id, t2)), t1), tokens'''')
        | _ -> raise (SyntaxError "Expected 'in'")
    )
    (* Lambda expression *)
    | LParen :: LambdaTok :: Literal id :: DotTok :: tokens' ->
        let t, tokens'' = parse_term tokens' in (
        match tokens'' with
        | RParen :: tokens''' -> (Abstraction (id, t), tokens''')
        | _ -> raise (SyntaxError "Expected right parenthesis")
    )
    (* Enclosed expression / Application expression*)
    | LParen :: tokens' ->
        let t1, tokens'' = parse_term tokens' in (
        match tokens'' with
        (* Enclosed expression *)
        | RParen :: tokens''' -> (t1, tokens''')
        (* Application expression *)
        | _ ->
            let t2, tokens''' = parse_term tokens'' in
            match tokens''' with
            | RParen :: tokens'''' -> (Application (t1, t2), tokens'''')
            | _ -> raise (SyntaxError "Expected right parenthesis")
    )
    (* Error *)
    | [] -> raise (SyntaxError "Expected tokens")
    | _ -> raise (SyntaxError "Unexpected token")

let parse str =
    let term, tokens = parse_term (tokenize (string_to_list str)) in
    match tokens with
    | [] -> (term)
    | _ -> raise (SyntaxError "Unexpected tokens")

let rec format_term = function
    | Variable v -> v
    | Abstraction (id, t) -> "(\\" ^ id ^ ". " ^ (format_term t) ^ ")"
    | Application (t1, t2) -> "(" ^ (format_term t1) ^ " " ^ (format_term t2) ^ ")"

(* 
** Bonus 
*)

(* Parse an 'atmoic' (non-application) term *)
let rec parse_term_conv_ = function
    (* Variable *)
    | Literal id :: tokens' -> (Variable id, tokens')
    (* Let expression *)
    | LetTok :: Literal id :: EqTok :: tokens' ->
        let t1, tokens'' = parse_term_conv tokens' in (
        match tokens'' with
        | InTok :: tokens''' ->
            let t2, tokens'''' = parse_term_conv tokens''' in
            (Application ((Abstraction (id, t2)), t1), tokens'''')
        | _ -> raise (SyntaxError "Expected 'in'")
    )
    (* Lambda expression *)
    | LambdaTok :: Literal id :: DotTok :: tokens' ->
        let t, tokens'' = parse_term_conv tokens' in (
        (Abstraction (id, t), tokens'')
    )
    (* Enclosed expression *)
    | LParen :: tokens' ->
        let t1, tokens'' = parse_term_conv tokens' in (
        match tokens'' with
        | RParen :: tokens''' -> (t1, tokens''')
        | _ -> raise (SyntaxError "Expected right parenthesis")
    )
    (* Error *)
    | [] -> raise (SyntaxError "Expected tokens")
    | _ -> raise (SyntaxError "Unexpected token")


(* Parse as many atomic terms as possible *)
and parse_terms_conv tokens =
    try
        let term, tokens' = parse_term_conv_ tokens in
        let terms, tokens'' = parse_terms_conv tokens' in
        (term :: terms, tokens'')
    with SyntaxError _ -> [], tokens

(* Parse single (not necessarily atomic) term *)
and parse_term_conv tokens =
    let terms, tokens' = parse_terms_conv tokens in
    match terms with
    (* Parse multiple atomic terms a single sequence of applications *)
    | term :: terms' -> (List.fold_left (fun t1 t2 -> Application (t1, t2)) term terms', tokens')
    | [] -> (* This means there was an error parsing a term, and it was caught in parse_terms_conv.
               Naturally we should raise an exception here. In order to have a helpful message with
               the exection, we'll simply call parse_term_conv_, which should raise an exeption. *)
            parse_term_conv_ tokens

(* Note: this function isn't part of the exercise, but is here for testing *)
let parse_conv str =
    let term, tokens = parse_term_conv (tokenize (string_to_list str)) in
    match tokens with
    | [] -> (term)
    | _ -> raise (SyntaxError "Unexpected tokens")

let rec format_term_conv = function
    | Variable v -> v
    | Abstraction (id, t) -> "\\" ^ id ^ ". " ^ (format_term_conv t)
    | Application (t1, t2) ->
	let formatted_t1 = format_term_conv t1 in
        let formatted_t2 = format_term_conv t2 in
        (* If t1 is Abstraction, then enclose it with parenthesis *)
        let formatted_t1' =
            match t1 with
            | Abstraction _ -> "(" ^ formatted_t1 ^ ")"
            | _ -> formatted_t1
            in
        (* If t2 is Application, then enclose it with parenthesis *)
        let formatted_t2' =
            match t2 with
            | Application _ -> "(" ^ formatted_t2 ^ ")"
            | _ -> formatted_t2
            in
        formatted_t1' ^ " " ^ formatted_t2'

