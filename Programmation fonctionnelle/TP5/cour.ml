let pr_int x = Printf.printf "%d" x
let pr_int_list l = List.iter pr_int l

let pr_int_list1 l = 
  let pr_int1 x = Printf.printf "%d" x in 
  List.iter pr_int1 l 

(*Fonctions anonymes*)
let print_int_list2 l = 
  List.iter(fun x -> Printf.printf "%d" x) l

let pr_float_list l = 
  List.iter(fun x -> Printf.printf "%f" x) l

let pr_int_int_list l = 
  List.iter(fun x -> Printf.printf "<%d, %d>" (fst x) (snd x)) l

(*List.iter*)
let a = List.iter (fun x -> Printf.printf "%b\n" x)[true;false;true]


(*List.filter: renvoie une liste de 
  tous les éléments qui remplissent
  une certaine condition
*)
let x = List.filter(fun x -> x mod 2 = 0) [4;5;42;1;37;49]
let y = List.filter(fun x -> x < 25.0) [10.5;2.3; 99.0]

(*List.map:Permet d'appliquer une transformation à 
  chaque élément d'une liste et 
  de renvoyer la liste des images 
*)
let b = List.map(fun x -> x * x) [4;8;3]
let c = List.map string_of_int [1;2;3]

let rec map f l = 
  match l with 
  [] -> []
  | x :: ll -> f x :: map f ll 

let f x = x * x

(*List.fold_left:Permet d'appliquer une fonction 
  d'agrégation aux éléments d'une liste 
*)

let d = List.fold_left (fun a x -> a + x) 0 [1;3;7]
let f a x = a ^ " " ^ string_of_int x
let e = List.fold_left f " " [1;3;7]
(*
  List.fold_left f "" [ 1; 3; 7 ]
  f (f (f "" 1) 3) 7
  f (f "1 " 3) 7
  f "1 3 " 7
  f "1 3 7 "
*)
let reversed_list l = 
  List.fold_left (fun acc x -> x :: acc) [] l 

let concatene_list lst = 
  List.fold_left (fun a x -> a ^ x) "" lst

(*List.sort: 
  -Prend en argument une fonction de comparaison.
  -La fonction de comparaison prend en argument deux 
  valeur a et b et renvoie un nombre négatif 
  (a < b), nul (a = b) ou positif (a > b)
*)

let x = List.sort compare [4;8;3]
let y = List.sort compare ["C"; "A"; "B"; "AX"]