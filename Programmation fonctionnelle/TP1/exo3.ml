let rec somme_entiers n = 
    if n = 0 then 0
    else 
        if n = 1 then 1
        else
        n + somme_entiers(n - 1)


let rec somme_carres n = 
    if n = 0 then 1
    else
        if n = 1 then n
        else
            n * n + somme_carres(n - 1)
let rec leibniz n =
    (* attention, on calcule sur des flottants *)
    if n = 0 then 1.0 
    else
        let s = if n mod 2 == 0 then 1.0 else -1.0 in
        s *. (1.0 /. float (2*n+1)) +. leibniz (n-1)

(*Test de fonction recursive*)
let () = Printf.printf "Donnez un nombre?\n"
let a = read_int()
let () = Printf.printf "La somme de a est:  %d\n"(somme_entiers a)        
let () = Printf.printf "Donnez un deuxieme nombre?\n"
let b = read_int()
let() = Printf.printf "La somme de carres de b est %d\n" (somme_carres b);
Printf.printf ""
(*Test sur fonction leibniz*)
let () = Printf.printf "4. *. leibniz 10 = %f\n"  (4. *. leibniz 10)
let () = Printf.printf "4. *. leibniz 50 = %f\n"  (4. *. leibniz 50)
let () = Printf.printf "4. *. leibniz 100 = %f\n"  (4. *. leibniz 100)
let () = Printf.printf "4. *. leibniz 500 = %f\n"  (4. *. leibniz 500)