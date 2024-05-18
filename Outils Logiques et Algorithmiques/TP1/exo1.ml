let est_pair n = 
  if n mod 2 = 0 then true
  else false 

let puiss1 k n = 
  let rec loop acc k n = 
    if n = 0 then acc 
    else loop (acc * k) k (n - 1)
  in 
  loop 1 k n 

let rec puiss2 k n = 
  if n = 0 then 1
  else if n > 0 && est_pair(n) then 
    let temp = puiss2 k (n / 2) in temp * temp
  else if n > 0 && not ( est_pair(n)) then 
    let temp = puiss2 k (n / 2) in k * (temp * temp)
  else failwith "Erreur: n doit être un entier positif"

let _ =
  Printf.printf "Le resultat de 2^3 est: %d\n"  (puiss2 2 3)