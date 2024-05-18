let print_e x = Printf.printf "%d" x
let pr_list sep print_e l = 
  match l with 
  [] -> ()
  |[x] -> print_e x
  |e1::ll -> 
    print_e e1;
    List.iter (fun x -> print_string(sep ^ " "); print_e x) ll

let pr_int_list lst = pr_list ", " (fun l -> pr_list "; " print_int l) lst

(*List.filter*)
let rec filter cond l = 
  match l with 
  |[] -> []
  |e1 :: ll -> 
    if cond e1 then e1 :: filter cond ll  
    else filter cond ll
    
(*List.map*)
let rec map f l = 
  match l with 
  |[] -> []
  |e1::ll -> f e1 :: map f ll

(*List.fold_left*)
let rec fold_left f acc l = 
  match l with 
  |[] -> acc 
  |e1 :: ll -> fold_left f (f acc e1 ) ll
(*List.rev*)
let renverse l = 
  let rec renverse_l l acc = 
    match l with 
    [] -> acc 
    |x :: ll -> renverse_l ll (x :: acc)
  in 
  renverse_l l []

(*List.rev en utilisant List.fold_left*)  
let rev l = 
  List.fold_left(fun acc x -> x :: acc) [] l

(*Écrire List.iter en utilisant List.fold_left*)
let iter func l = 
  List.fold_left (fun () x -> func ) () l 

(*Écrire List.map en utilisant List.fold_left et List.rev*)
let map f l = 
  let mapper acc x = f x :: acc in 
  List.rev(List.fold_left mapper [] l)

(*Insérer une valeur au bon endroit dans une liste triée*)
let comp a b = a - b
let rec insert comp x l = 
  match l with 
  | [] -> [x]
  | e1 :: ll -> 
    if comp x e1 <= 0 then x :: l 
    else e1 :: insert comp x ll 

(*Retirer les doublons d'une liste triée*)
let rec uniq comp l = 
  match l with 
  [] -> []
  | [e] -> [e]
  | e1 :: e2 :: ll -> 
    if comp e1 e2 = 0 then uniq comp (e2 :: ll)
    else e1::(uniq comp (e2::ll))

let rec quick_sort comp l = 
  match l with 
  [] -> []
  | [x] -> [x]
  | pivot :: ll -> 
    let rec partiton lt lg = 
      match lg with 
      [] -> (quick_sort comp lt) @ [pivot]
      | e1 :: ll -> 
        if comp e1 pivot <= 0 then partiton (e1 :: lt) ll
        else partiton lt ll 
    in
    let(less, greater ) = List.partition( fun x -> comp x pivot <= 0 ) ll in 
    (quick_sort comp less) @ [pivot] @ (quick_sort comp greater) 