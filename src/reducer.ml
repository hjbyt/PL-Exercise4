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
    | Abstraction (v, t) ->  StringSet.diff (fv t) (StringSet.singleton v)
    | Application (t1, t2) ->  StringSet.union (fv t1) (fv t2)

let possible_variables_set = StringSet.of_list  possible_variables

let fresh_var used_vars = let candidates = StringSet.diff possible_variables_set used_vars in
    if StringSet.is_empty candidates
    then raise OutOfVariablesError
    else StringSet.choose candidates

let rec substitute var replacement term = match term with
    | Variable v -> if v = var
                    then replacement
                    else Variable v
    | Application (t1, t2) -> Application (substitute var replacement t1, substitute var replacement t2)
    | Abstraction (v, t) -> if v = var
                            then Abstraction (v, t)
                            else if not (StringSet.mem v (fv replacement))
                                 then Abstraction (v, substitute var replacement t)
                                 else let z = fresh_var (List.fold_left StringSet.union StringSet.empty [fv replacement; fv t; StringSet.singleton v]) in
                                      Abstraction (z, substitute var replacement (substitute v (Variable z) t))


let apply_abs t1 t2 = match  t1 with
    | Abstraction (v, t) -> Some (substitute v t2 t)
    | _ -> None

let rec reduce_strict = function
    | Variable v -> None
    | Abstraction (_, _) -> None
    | Application (t1, t2) -> (* Try E-App1 *)
                              let reduced_t1 = reduce_strict t1 in
                              match reduced_t1 with
                              | Some t1' -> Some (Application (t1', t2))
                              | None -> (* Try E-App2 *)
                                        let reduced_t2 = reduce_strict t2 in
                                        match reduced_t2 with
                                        | Some t2' -> Some (Application (t1, t2'))
                                        | None -> (* Try E-AppAbs *)
                                                  apply_abs t1 t2

let reduce_lazy = function
    | Variable v -> None
    | Abstraction (_, _) -> None
    | Application (t1, t2) -> (* Try E-App1 *)
                              let reduced_t1 = reduce_strict t1 in
                              match reduced_t1 with
                              | Some t1' -> Some (Application (t1', t2))
                              | None -> (* Try E-AppAbs *)
                                        apply_abs t1 t2

let reduce_normal term = None

