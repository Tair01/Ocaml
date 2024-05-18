(*Exercice 1*)
type binop = And | Or | Imp 
type fmla = 
  | True
  | False
  | Var of int 
  | Not of fmla 
  | Bin of binop * fmla * fmla 

(*Question 1*)
let rec contains i f = 
  match f with 
  | True | False -> false
  | Var x -> x = i
  | Not ff -> contains i ff
  | Bin (_, f1, f2) -> contains i f1 || contains i f2

(*Question 2*)
let rec elimp_imp f = 
  match f with 
  | True | False | Var _ -> f 
  | Not f -> Not (elimp_imp f)
  | Bin (Imp, f1, f2) -> Bin(Or, Not(elimp_imp f1), elimp_imp f2)
  | Bin (op, f1, f2) -> Bin(op, elimp_imp f1, elimp_imp f2)

(*Question 3*)
let rec is_nnf f = 
  match f with 
  | True | False | Var _ -> true 
  | Not(Var _) -> true 
  | Not _ -> false 
  | Bin(op, f1, f2) -> 
      match op with 
      |And | Or -> is_nnf f1 && is_nnf f2 
      |_ -> false
      
(* Question 4*)
let rec neg_nnf f = 
  match f with 
  | True -> False 
  | False -> True 
  | Var x -> Not (Var x)
  | Not f -> Not (Not((neg_nnf f)))
  | Bin (op, f1, f2) -> 
    match op with 
    | And -> Bin(Or, neg_nnf f1, neg_nnf f2)
    | Or -> Bin(And, neg_nnf f1, neg_nnf f2)
    | _ -> f

(*Question 5*)
let rec nnf f = 
  match f with 
  | True -> True
  | False -> False 
  | Var x -> Not(Var x)
  | Not f -> neg_nnf (nnf f)
  | Bin(op, f1, f2) -> 
    match op with 
    | And -> Bin(And, nnf f1, nnf f2)
    | Or -> Bin(Or, nnf f1, nnf f2)
    | Imp -> elimp_imp f

(*Exercice 2*)
let s_not f = match f with 
    | True -> False
    | False -> True 
    | Not f' -> f' 
    | _ -> Not f 

(* Question 1*)
let s_and f1 f2 = match f1, f2 with 
  | True, _ -> f2 
  | _, True ->  f1
  | False, _ -> False 
  | _, False -> False 
  | _ -> Bin(And, f1, f2)

let s_or f1 f2 = match f1, f2 with 
  | True, _ | _, True -> True 
  | False, _ -> f2
  | _, False -> f1
  | _ -> Bin(Or, f1, f2)

let s_imp f1 f2 = match f1,f2 with 
  | False, _ | _, True -> True 
  | True, _ -> f2
  | _, False -> Not f1 
  | _ -> Bin(Imp, f1,f2)

  