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
    | LetTok :: Literal id :: EqTok :: tokens' -> let t1, tokens'' = parse_term tokens' in (
        match tokens'' with
        | InTok :: tokens''' -> let t2, tokens'''' = parse_term tokens''' in
            (Application ((Abstraction (id, t2)), t1), tokens'''')
        | _ -> raise (SyntaxError "Expected 'in'\n")
    )
    (* Lambda expression *)
    | LParen :: LambdaTok :: Literal id :: DotTok :: tokens' -> let t, tokens'' = parse_term tokens' in (
        match tokens'' with
        | RParen :: tokens''' -> (Abstraction (id, t), tokens''')
        | _ -> raise (SyntaxError "Expected right parenthesis\n")
    )
    (* Enclosed expression / Application expression*)
    | LParen :: tokens' -> let t1, tokens'' = parse_term tokens' in (
        match tokens'' with
        (* Enclosed expression *)
        | RParen :: tokens''' -> (t1, tokens''')
        (* Application expression *)
        | _ -> let t2, tokens''' = parse_term tokens'' in
            match tokens''' with
            | RParen :: tokens'''' -> (Application (t1, t2), tokens'''')
            | _ -> raise (SyntaxError "Expected right parenthesis\n")
    )
    (* Error *)
    | _ -> raise (SyntaxError "Unexpected token\n")

let parse str = let term, tokens = parse_term (tokenize (string_to_list str)) in
    match tokens with
    | [] -> (term)
    | _ -> raise (SyntaxError "Unexpected tokens\n")

let rec format_term = function
    | Variable v -> v
    | Abstraction (id, t) -> "(\\" ^ id ^ "." ^ (format_term t) ^ ")"
    | Application (t1, t2) -> "(" ^ (format_term t1) ^ " " ^ (format_term t2) ^ ")"
