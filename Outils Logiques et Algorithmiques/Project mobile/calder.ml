(*Baurzhan Tair, G1, num: 22212381*)
(*Tous les test sont commentées*)
type mobile = 
  | O of int 
  | B of mobile * mobile 

(*  1. Mobiles équilibrés   *)
(*Question 1*)
let m1 = B(O(16), B(B(O(4), O(4)), O(8))) 
(*Question 2*)
let m = B ( O (32) , B ( B ( B ( B ( O (2) , O (2)) , O (4)) , B ( O (4) , B ( B ( O (1) , O (1)) , O (2)))) , O (16)))
(*
             |
      +------+-------+
      |              |
      32       +-----+-----+
               |           |
          +----+----+     16
          |         |   
      +---+---+ +---+---+
      |       | |       |   
   +--+--+    4 4    +--+---+
   |     |           |      |
   2     2        +--+--+   2
                  |     |
                  1     1
*)
(*Question 3*)
(*
La partie (a)
  Pour t0: 
                   |
                   1
  Pour t1: 
                   |
                +--+--+
                |     |
                1     1
  Pour t2: 
            	    |
	            +---+---+
	            |       |
           +--+--+ +--+--+
          |      | |     |
          1      1 1     1
  Pour t3:
                    |
           +--------+---------+
           |                  |
       +---+---+          +---+-----+
       |       |          |         |
    +--+--+ +--+--+    +--+--+   +--+--+
    |     | |     |    |     |   |     |
    1     1 1     1    1     1   1     1 
La partie (b) 
  La taille : 2^k
  La hauteur : k
  Le poids est :  2^k
*)
(*La partie (c)*)
let rec t k = match k with 
  | 0 -> O(1)
  | 1 -> B(O(1), O(1))
  | _ -> B(t (k-1), t (k - 1))

(*Les tests pour la partie c
let () = 
  Printf.printf "Question 3\n";
  Printf.printf "Résultat de t0: %b\n" (t 0 = O(1));
  Printf.printf "Résultat de t1: %b\n" (t 1 = B(O(1), O(1)));
  Printf.printf "Résultat de t2: %b\n" (t 2 = B(B(O(1), O(1)), B(O(1), O(1))));
  Printf.printf "Résultat de t3: %b\n" (t 3 =B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1)))));
  Printf.printf "\n"
*)

(*Question 4*)
(*La partie (a)
  Pour u0 : 
                 |
                 1
  Pour u1 : 
                 |
              +--+--+
              |     |
              1     1
  Pour u2: 
                   |
	           +-----+-----+
             |           |
             2      +--+--+
  		              |     |
 		                1     1	
  Pour u3: 
                     |
	             +-----+-----+
               |           |
               4        +--+---+
  		                  |      |
 		                    2   +--+--+	
		                        |     |
                            1     1
La partie (b)
  La taille est : k + 1
  La hauteur est : k
  Le poids est : 2^k
*)
(*La partie (c)*)
let rec u k = match k with 
  | 0 -> O(1)
  | 1 -> B(O(1), O(1))
  | _ -> B(O(int_of_float(2.0 ** float_of_int (k - 1))), u (k - 1))

(*Les tests pour la partie (c)
let () =  
  Printf.printf "Question 4\n";
  Printf.printf "Résultat de u0: %b\n" (u 0 = O(1));
  Printf.printf "Résultat de u1: %b\n" (u 1 = B(O(1), O(1)));
  Printf.printf "Résultat de u2: %b\n" (u 2 = B(O(2), B(O(1), O(1))));
  Printf.printf "Résultat de u3: %b\n" (u 3 = B(O(4), B(O(2), B(O(1), O(1)))));
  Printf.printf "Résultat de u4: %b\n" (u 4 = B(O(8), B(O(4), B(O(2), B(O(1), O(1)))))); (*Un test supp pour u4*)
  Printf.printf "\n"
*)

(*Question 5
  Montrons par récurrence structurelle sur m que barres (m) = |m| - 1
  Supposons que la propriété est vraie pour tous les mobilles de taille a,
  ou 1 <= a <= k (HR)
  Cas de base: |m| = 1, m contient qu'un seul objet qui est O(un nombre),
  qui ne contient pas de barres. Le nombre de barres pour |m| = 1 est: 
  |m| - 1 = 1 - 1 = 0, donc c'est vraie
  Soit un mobile m de taille k + 1, alors m -> B(m1, m2), ou m1 et m2 sont
  des sous mobiles de m, par HR m1 a |m1| - 1 barres et m2 a |m2| - 1 barres, 
  comme m1 et m2 sont des sous mobiles de m, m contient une barre supp pour faire
  relier m1 et m2, donc on a le nombre de barres dans m est (|m1| - 1) + (|m2| - 1) + 1
  = |m| - 1. Donc c'est vraie pour tout mobile m de taille |m|.
*)

(*Question 6
Outre la taille, la structure de l'arbre peut jouer un rôle important, surtout pour les arbres binaires. 
Si on prends l'exemple de l'arbre binaire de type décrit dans les questions 3 ou 4, où la hauteur de l'arbre est particulièrement pertinente. 
Donc je deduis que parcourir un arbre parfait tq celui de la question 3 peut être plus efficace et plus facile que parcourir un arbre complètement
aléatoire .
*)

(*Question 7*)
let rec size m =  match m with 
  | O(_) -> 1
  | B(m1,m2) -> size m1 + size m2 

(*Les tests pour la fonction size 
let () = 
  Printf.printf "Question 7, la fonction size\n";
  Printf.printf "Résultat de size pour t0: O(1): %b\n" (size (O(1)) = 1);
  Printf.printf "Résultat de size pour t1: B(O(1), O(1)): %b\n" (size (B(O(1), O(1))) = 2);
  Printf.printf "Résultat de size pour t2: B(B(O(1), O(1)), B(O(1), O(1))): %b\n" (size (B(B(O(1), O(1)), B(O(1), O(1)))) = 4);
  Printf.printf "Résultat de size pour t3: B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1)))): %b\n" (size (B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1))))) = 8);
  Printf.printf "\n";
  Printf.printf "Résultat de size pour u0: O(1): %b\n" (size (O(1)) = 1);
  Printf.printf "Résultat de size pour u1: B(O(1), O(1)): %b\n" (size (B(O(1), O(1))) = 2);
  Printf.printf "Résultat de size pour u2: B(O(2), B(O(1), O(1))): %b\n" (size (B(O(2), B(O(1), O(1)))) = 3);
  Printf.printf "Résultat de size pour u3: B(O(4), B(O(2), B(O(1), O(1)))): %b\n" (size (B(O(4), B(O(2), B(O(1), O(1))))) = 4);
  Printf.printf "Résultat de size pour u4: B(O(8), B(O(4), B(O(2), B(O(1), O(1))))): %b\n" (size (B(O(8), B(O(4), B(O(2), B(O(1), O(1)))))) = 5);
  Printf.printf "\n"
*)
  
let rec height m  = match m with 
  | O(_) -> 0 
  | B(m1, m2) -> 1 + max( height m1) (height m2)

(*Les tests pour la fonction height 
let () = 
  Printf.printf "Question 7, la fonction height\n";
  Printf.printf "Résultat de height pour t0: O(1): %b\n" (height (O(1)) = 0);
  Printf.printf "Résultat de height pour t1: B(O(1), O(1)): %b\n" (height (B(O(1), O(1))) = 1);
  Printf.printf "Résultat de height pour t2: B(B(O(1), O(1)), B(O(1), O(1))): %b\n" (height (B(B(O(1), O(1)), B(O(1), O(1)))) = 2);
  Printf.printf "Résultat de height pour t3: B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1)))): %b\n" (height (B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1))))) = 3);
  Printf.printf "\n";
  Printf.printf "Résultat de height pour u0: O(1): %b\n" (height (O(1)) = 0);
  Printf.printf "Résultat de height pour u1: B(O(1), O(1)): %b\n" (height (B(O(1), O(1))) = 1);
  Printf.printf "Résultat de height pour u2: B(O(2), B(O(1), O(1))): %b\n" (height (B(O(2), B(O(1), O(1)))) = 2);
  Printf.printf "Résultat de height pour u3: B(O(4), B(O(2), B(O(1), O(1)))): %b\n" (height (B(O(4), B(O(2), B(O(1), O(1))))) = 3);
  Printf.printf "Résultat de height pour u4: B(O(8), B(O(4), B(O(2), B(O(1), O(1))))): %b\n" (height (B(O(8), B(O(4), B(O(2), B(O(1), O(1)))))) = 4);
  Printf.printf "\n"
*)

let rec weight m =  match m with 
  | O(n) -> n
  | B(m1, m2) -> weight m1 + weight m2 

(*Les tests pour la fonction weight
let () = 
  Printf.printf "Question 7, la fonction weight\n";
  Printf.printf "Résultat de weight pour t0: O(1): %b\n" (weight (O(1)) = 1);
  Printf.printf "Résultat de weight pour t1: B(O(1), O(1)): %b\n" (weight (B(O(1), O(1))) = 2);
  Printf.printf "Résultat de weight pour t2: B(B(O(1), O(1)), B(O(1), O(1))): %b\n" (weight (B(B(O(1), O(1)), B(O(1), O(1)))) = 4);
  Printf.printf "Résultat de weight pour t3: B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1)))): %b\n" (weight (B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1))))) = 8);
  Printf.printf "\n";
  Printf.printf "Résultat de weight pour u0: O(1): %b\n" (weight (O(1)) = 1);
  Printf.printf "Résultat de weight pour u1: B(O(1), O(1)): %b\n" (weight (B(O(1), O(1))) = 2);
  Printf.printf "Résultat de weight pour u2: B(O(2), B(O(1), O(1))): %b\n" (weight (B(O(2), B(O(1), O(1)))) = 4);
  Printf.printf "Résultat de weight pour u3: B(O(4), B(O(2), B(O(1), O(1)))): %b\n" (weight (B(O(4), B(O(2), B(O(1), O(1))))) = 8);
  Printf.printf "Résultat de weight pour u4: B(O(8), B(O(4), B(O(2), B(O(1), O(1))))): %b\n" (weight (B(O(8), B(O(4), B(O(2), B(O(1), O(1)))))) = 16);
  Printf.printf "\n"
*)


(*Question 8
La partie (a)
  Si on compte la comparaison de poids comme une opération et un calcul de poids pour chaque branche de l'arbre, 
  alors on obtient une opération pour chaque branche en dessous de cette arbre. Ainsi, l'ordre de grandeur du nombre total 
  d'opérations est proportionnel à |m|^2 ou 2^2k, où |m| représente le poids total du mobile.

La partie (b)
  Le nombre total d'opérations est proportionnel à |m|^2 ou k^2, où |m| représente le poids total du mobile
*)

let rec balanced = function
  | O _ -> true
  | B ( t1 , t2 ) -> balanced t1 && balanced t2 && weight t1 = weight t2

(*Les tests pour la fonction balanced 
let () =
  Printf.printf "Tests pour la fonction balanced :\n";
  Printf.printf "Résultat de balanced pour t0: O(1) : %b\n" (balanced (O(1)) = true);
  Printf.printf "Résultat de balanced pour t1: B(O(1), O(1)) : %b\n" (balanced (B(O(1), O(1))) = true);
  Printf.printf "Résultat de balanced pour t2: B(B(O(1), O(1)), B(O(1), O(1))) : %b\n" (balanced (B(B(O(1), O(1)), B(O(1), O(1)))) = true);
  Printf.printf "Résultat de balanced pour t3: B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1)))) : %b\n" (balanced (B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1))))) = true);

  Printf.printf "Résultat de balanced pour u0: O(1) : %b\n" (balanced (O(1)) = true);
  Printf.printf "Résultat de balanced pour u1: B(O(1), O(1)) : %b\n" (balanced (B(O(1), O(1))) = true);
  Printf.printf "Résultat de balanced pour u2: B(O(2), B(O(1), O(1))) : %b\n" (balanced (B(O(2), B(O(1), O(1)))) = true);
  Printf.printf "Résultat de balanced pour u3: B(O(4), B(O(2), B(O(1), O(1)))) : %b\n" (balanced (B(O(4), B(O(2), B(O(1), O(1))))) = true);
  Printf.printf "Résultat de balanced pour u4: B(O(8), B(O(4), B(O(2), B(O(1), O(1))))) : %b\n" (balanced (B(O(8), B(O(4), B(O(2), B(O(1), O(1)))))) = true);
  Printf.printf "\n"
*)


(*Question 9*)
(*Fonction balanced' est récursive terminale *)
let balanced' arb = 
  let rec loop a =  match a with 
      |O(r) -> r
      |B(m1, m2) ->
        let t1 = (loop m1) in 
        let t2 = (loop m2) in 
        if t2!= -1 &&  t1 = t2 && t1 != -1 then t2 + t1 
        else -1 in if(loop arb) = -1 then false else true

(*Les tests pour la fonction balanced' 
let () =
  Printf.printf "Tests pour la fonction balanced' :\n";
  Printf.printf "Résultat de balanced' pour t0: O(1) : %b\n" (balanced' (O(1)) = true);
  Printf.printf "Résultat de balanced' pour t1: B(O(1), O(1)) : %b\n" (balanced' (B(O(1), O(1))) = true);
  Printf.printf "Résultat de balanced' pour t2: B(B(O(1), O(1)), B(O(1), O(1))) : %b\n" (balanced' (B(B(O(1), O(1)), B(O(1), O(1)))) = true);
  Printf.printf "Résultat de balanced' pour t3: B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1)))) : %b\n" (balanced' (B(B(B(O(1), O(1)), B(O(1), O(1))), B(B(O(1), O(1)), B(O(1), O(1))))) = true);

  Printf.printf "Résultat de balanced' pour u0: O(1) : %b\n" (balanced' (O(1)) = true);
  Printf.printf "Résultat de balanced' pour u1: B(O(1), O(1)) : %b\n" (balanced' (B(O(1), O(1))) = true);
  Printf.printf "Résultat de balanced' pour u2: B(O(2), B(O(1), O(1))) : %b\n" (balanced' (B(O(2), B(O(1), O(1)))) = true);
  Printf.printf "Résultat de balanced' pour u3: B(O(4), B(O(2), B(O(1), O(1)))) : %b\n" (balanced' (B(O(4), B(O(2), B(O(1), O(1))))) = true);
  Printf.printf "Résultat de balanced' pour u4: B(O(8), B(O(4), B(O(2), B(O(1), O(1))))) : %b\n" (balanced' (B(O(8), B(O(4), B(O(2), B(O(1), O(1)))))) = true);
  Printf.printf "\n"
*)

(*  2. Dimensionnement des éléments   *)
let h = 1

type annot_mobile =
| AO of int (* AO(r) *)
| AB of int * int * annot_mobile * annot_mobile (* AB(rx , ry , m1 , m2) *)

let sqrt n =
  let rec loop r s =
  (* Precondition : s = r*r *)
  if s > n then r - 1
  else loop ( r +1) ( s +2* r +1)
  in
  loop 0 0
  
let unit_radius = 4
let sq_unit = unit_radius * unit_radius
let radius w = sqrt ( sq_unit * w )
let width = 2 * h
  
(* Question 10
  Démontrons par récurrence que tous les appels à la fonction auxiliaire loop respectent 
  la précondition  de l'énoncée dans notre  code. Je suppose que pour chaque itération de la 
  fonct loop, la valeur de s est toujours <= n. Cas de base : Pour r = 0, la valeur de s est 
  egal à 0, ce qui est toujours <= n, car n est un entier positif qui est non nul, donc c'est vraie.
  Maintenant je suppose que pour une certaine valeur de r = k, la précondition est respectée,
  cad que s = k^2 est toujours <= n. Je dois démontrer que la précondition est vraie pour r = k + 1. 
  Dans la fonction loop, s est mis à jour en ajoutant 2*r + 1 à chaque itération. Alors pour r = k + 1, 
  s devient s + 2*(k + 1) + 1 = s + 2k + 3, car la précondition est respectée pour r = k, cad s = k^2 est toujours <=n, et 
  que k^2 est un carré parfait, k^2 + 2k + 3 est toujours < (k + 1)^2 pour tout k. Donc je déduis que la précondition est 
  respectée pour toutes les valeurs de r, ce qui signifie que tous les appels à la fonction loop respectent bien 
  la précondition de l'énoncée.
*)
(* Question 11
  On n'a pas défini radius w par l'expression plus simple unit_radius * sqrt w en raison que cela ne va pas garantir
  une taille proportionnelle correcte des objets en fonct de leur poids. Le but principal de la fonction radius est 
  de calculer le rayon d'un objet en fonct de son poids tq la taille des objets soit proportionnelle à  la sqrt 
  de leur poids. En utilisant unit_radius * sqrt w, nous ne tiendrions pas compte de la dimension physique 
  des objets. Je pense qu cette expression ne prend pas en compte que la taille des objets doit augmenter de manière non linéaire 
  en fonction de leur poids. En utilisant l'expression unit_radius * sqrt w, la taille des objets augmenterait de manière linéaire 
  avec le poids, ce qui ne correspond pas à la façon dont les objets devraient être dimensionnés d'après l'énoncée. La fonction 
  radius utilise plutôt une approche basée sur la sqrt pour garantir une relation proportionnelle entre la taille des objets et leur poids.
  Donc, unit_radius * sqrt w ne donnerait pas une dimension appropriée pour les objets par rapport à leur poids*)

(*Question 12*)
let uniform_width w = let t = 2 * (h + (radius 1)) in w * t 

(*Question 13*)
let rec annot_uniform m = match m with
  | O(n) -> AO(radius n)
  | B(m1, m2) ->
      let a = uniform_width (weight m1) / 2 in 
      let b = radius ((weight m1) + (weight m2)) in 
      let annot_m1 = (annot_uniform m1) in 
      let annot_m2 = (annot_uniform m2 ) in 
      AB(a, b, annot_m1, annot_m2)

(* Tests  
let () = 
    Printf.printf "Tests pour la fonction annot_uniform :\n";
    Printf.printf "Résultat de annot_uniform pour B ( B ( O (1) , O (1)) , O (2)): %b\n" (annot_uniform (B ( B ( O (1) , O (1)) , O (2))) = AB (10 , 8 , AB (5 , 5 , AO (4) , AO (4)) , AO (5)));
    Printf.printf "\n"
*)
    
(*Question 14*) (*Faut revoir*)
let rec sp_m m= match m with
  |O(r) -> 2*h+2*(radius r) 
  |B(m1, m2) -> (sp_m m1)+(sp_m m2)

let rec left_width a = match a with 
  | O(r) -> (2 * h + (2 * (radius r))) / 2
  | B(m1, m2) -> 
    let left_m1 = left_width m1 in
    let sep_m1_m2 = sp_m m1 in (left_m1 + sep_m1_m2) / 2

let rec right_width a = match a with 
  | O(r) -> (2 * h + (2 * (radius r))) / 2 
  | B(m1, m2) -> 
    let right_m2 = right_width m2 in
    let sep_m1_m2 = sp_m m1 in (sep_m1_m2 - right_m2) / 2
    
(* Question 15 *) (*Faut revoir*)
let rec annot_sep m = match m with 
  | O(n) -> AO(radius n)  
  | B(m1, m2) -> 
    let a = (left_width m1) + (right_width m2) / 2 in
    let b = radius ((weight m1) + (weight m2)) in 
    let annot_m1 = (annot_sep m1) in
    let annot_m2 = (annot_sep m2) in  
    AB(a, b, annot_m1, annot_m2)  

(* TEST 
let () = 
    Printf.printf "Tests pour la fonction annot_sep :\n";
    Printf.printf "Résultat de annot_sep pour B ( B ( O (1) , O (1)) , O (2)): %b\n" (annot_sep (B ( B ( O (1) , O (1)) , O (2)))=(AB (8 , 8 , AB (5 , 5 , AO (4) , AO (4)) , AO (5))));
    Printf.printf "\n"
*)

(*Question 16*)
let rec concat l1 l2 = match l1, l2 with
  | [], [] -> []
  | hd1::tl1, [] -> hd1 :: (concat tl1 [])
  | [], hd2::tl2 -> hd2 :: (concat [] tl2)
  | hd1::tl1, hd2::tl2 -> (max hd1 hd2) :: (concat tl1 tl2)

let rec left_profile b m = match m with 
  |AO(r) -> [b + r + h] 
  |AB(x, y, m1, m2)->  
    let a = [x + b + h] in 
    let c = (concat (left_profile (x+b) m1) (left_profile (b-x) m2)) in a @ c

let rec right_profile b m = match m with 
  |AO(r) -> [b + r + h] 
  |AB(x, y, m1, m2)->  
    let a = [x + b + h] in
    let c = (concat (right_profile (b-x) m1) (right_profile (b+x) m2)) in a @ c

(*Les TESTS 
let () = 
  let am = AB (6, 8, AB (5, 5, AO (4), AO (4)), AO (5)) in 
  let test_left = left_profile 0 am in 
  let test_right = right_profile 0 am in 
  Printf.printf "Tests pour les fonctions left, right_profile :\n";
  Printf.printf "Résultat de left_profile pour AB (6, 8, AB (5, 5, AO (4), AO (4)), AO (5)): %b\n" (test_left = [7; 12; 16]);
  Printf.printf "Résultat de right_profile pour AB (6, 8, AB (5, 5, AO (4), AO (4)), AO (5)): %b\n" (test_right = [7; 12; 4]);
  Printf.printf "\n"
*)

(*Question 17 *)
let get_distance r1 l x =
  let rec loop r1 l1 x1 d = match r1, l1 with 
    | [], _ | _, [] -> d (*Case ede base: si l'un des profils est vide, on renvoie la distance actuelle *)
    | hd1::tl1, hd2::tl2 ->
        let new_d = ((x1 - hd1) + (x1 - hd2)) / 2 in
        if d <> -1 then min d (loop tl1 tl2 x1 new_d)
        else loop tl1 tl2 x1 new_d in loop r1 l x (-1)

let rec annot_nest m = 
  let new_m = annot_sep m in 
  let rec loop m = match m with 
  | AO(r) -> AO(r)
  | AB(x, y, m1, m2) -> 
    let new_x = x - (get_distance (left_profile 0 m1) (right_profile 0 m2) x) in 
    let annot_m1 = loop m1 in 
    let annot_m2 = loop m2 in  
    AB(new_x, y, annot_m1, annot_m2) in loop new_m

(* TESTS
let() = 
  let m = B ( B ( O (1) , O (1)) , O (2)) in
  let annot_m = annot_nest m in 
  Printf.printf "Tests pour la fonction annot_nest :\n";
  Printf.printf "Résultat de annot_nest pour B ( B ( O (1) , O (1)) , O (2)): %b\n" (annot_m = AB (6 , 8 , AB (5 , 5 , AO (4) , AO (4)) , AO (5)));
  Printf.printf "\n"
*)

open Printf
(* Objet , centre (cx ,cy) et rayon r *)
let print_object cx cy r =
printf "<circle cx =\"%d\" cy =\"%d\" r =\"%d\" fill =\"black\"/>\n" cx cy r
(* Barre horizontale , extremite gauche (x1 ,y1) et droite (x2 ,y2) *)
let print_bar x1 y1 x2 y2 =
printf
"<line x1 =\"%d\" y1 =\"%d\" x2 =\"%d\" y2 =\"%d\" stroke-width =\"2px\" stroke =\"black\"/>\n"
( x1 -1) y1 ( x2 +1) y2
(* Fil vertical , entre (x1 ,y1) et (x2 ,y2) *)
let print_thread x1 y1 x2 y2 =
printf "<line x1 =\"%d\" y1 =\"%d\" x2 =\"%d\" y2 =\"%d\" stroke =\"black\"/>\n" x1 y1 x2 y2
(* Affichage d'un mobile *)
let generate_svg printer t =
printf "<svg xmlns =\"http://www.w3.org/2000/svg\">\n" ;
printer t ;
printf "</svg>\n"

(*Question 18*)
let rec print_annot_mobile x y m = match m with 
  |AO(r) -> (print_object x (y+r) r)
  |AB(x1, y1, m1, m2) -> (print_bar (x-x1) (y+y1) (x+x1) (y+y1));
  print_thread x y x (y+y1);
  print_thread (x-x1) (y+y1) (x-x1) (y+y1*2);
  print_thread (x+x1) (y+y1) (x+x1) (y+y1*2);
  print_annot_mobile (x-x1) (y+2*y1) m1;
  print_annot_mobile (x+x1) (y+2*y1) m2

(*Question 19*)
let print_mobile m =
  let annot_m = annot_nest m in
  generate_svg (fun m -> print_annot_mobile 100 100 m) annot_m

(* TEST *)
let new_m = B ( O (32) , B ( B ( B ( B ( O (2) , O (2)) , O (4)) , B ( O (4) , B ( B ( O (1) , O (1)) , O (2)))) , O (16)))
(* TESTS 
let() = print_mobile new_m 
*)

(*Question 20*)
let rec gen_fission k = 
  if k = 0 then O(1)
  else if Random.int 10 < 3 then O(int_of_float (2. ** float_of_int k))
  else B(gen_fission (k - 1), gen_fission (k - 1))

(*Question 21*)
let rec gen_fusion k =
  let p = (float_of_int k) /. 20. in
  let rec loop k = match k with
    | 0 -> O(1)
    | _ ->
      if Random.float 1. < p then O(1)
      else B(loop (k - 1), loop (k - 1)) in
  let m1 = loop k in
  let m2 = loop k in
  match m1, m2 with
  | O(_), O(_) -> if Random.float 1. < p then O(1) else B(m1, m2)
  | _ -> B(m1, m2)

(* TEST 
let () = 
  print_mobile (gen_fusion 1);
  Printf.printf "\n"
*)
