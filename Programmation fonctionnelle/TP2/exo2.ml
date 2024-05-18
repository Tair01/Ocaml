(*La fonction lit un entier au clavier 
  au moyen de read_int () puis compare cet entier à n. Si 
  les deux sont égaux, la fonction
  affiche "Trouvé !". Sinon elle affiche "Trop petit!" ou 
  "Trop grand!" puis se rappelle récursivement*)
let rec guess n = 
  Printf.printf "Donnez un chiffre?\n";
  let r = read_int() in
  if r = n then Printf.printf "Trouvé!\n"
  else if r < n then begin 
    Printf.printf "Trop petit!\n";
    guess n
    end
  else if r > n then begin 
    Printf.printf "Trop grand!\n";
    guess n 
    end

(*Initialisation de Random*)
let () = 
    let () = Random.self_init() in
    let n = Random.int 101 in 
    guess n

(*Affiche avec des nombres d'essais*)
let rec guess1 n d = 
  Printf.printf "Donnez un chiffre?\n";
  let r = read_int() in 
  if r == n then Printf.printf "Trouvé! en %d essais!\n" (d + 1)
  else if r < n then begin 
      Printf.printf "Trop petit!\n";
      guess1 n (d + 1)
    end
  else if r > n then begin 
      Printf.printf "Trop grand!\n";
      guess1 n (d + 1)
    end

let () = 
  let () = Random.self_init () in 
  let n = Random.int 101 in 
  let d = 0 in 
  guess1 n d