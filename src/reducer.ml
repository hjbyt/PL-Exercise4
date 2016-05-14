(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))



(*
  ADD FUNCTIONS BELOW
*)
  
let rec fv = function
  | Variable v -> StringSet.singleton v
  | Abstraction (id, t) ->  StringSet.diff (fv t) ( StringSet.singleton id)
  | Application (t1, t2) ->  StringSet.union (fv t1) (fv t2)

let possible_variables_set = StringSet.of_list  possible_variables

let fresh_var used_vars = let candidates = StringSet.diff possible_variables_set used_vars in
	if StringSet.is_empty candidates
	then raise OutOfVariablesError
	else StringSet.choose candidates

let reduce_normal term = None
let reduce_strict term = None
let reduce_lazy term = None

