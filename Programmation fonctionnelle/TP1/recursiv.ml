(*Affiche de 1 à n*)
let rec affiche n = 
  if n > 0 then 
    affiche(n - 1);
    Printf.printf "%d " n

let rec ackermann m n =
  match (m, n) with
    | 0, _ -> n + 1
    | _, 0 -> ackermann (m - 1) 1
    | _, _ -> ackermann (m - 1) (ackermann m (n - 1))

