type piquet = string * int list
type jeu = piquet list

let affiche_piquet (nom, disque) = 
  Printf.printf "%s|" nom;
  let rec loop t1 = 
    let l = List.rev t1 in 
    match l with
    [] -> ()
    |x::ll ->
      Printf.printf "%d-" x;
      loop ll
  in
  loop disque;
  Printf.printf "\n"

(*Test*)
let () = affiche_piquet ("mid", [1;2;5]) 
let choix_piquet l nom = (nom, List.assoc nom l )


let piquets = [ ("dep", [2;3;4]);
                ("mid", []);
                ("arr", [1])] 
(*Test*)
let m = choix_piquet piquets "mid"


let rec affiche_jeu l   = 
  Printf.printf "\x1b[2J\x1b[H";
  affiche_piquet (choix_piquet l "dep" );
  affiche_piquet (choix_piquet l "mil");
  affiche_piquet (choix_piquet l "arr");
  Printf.printf "%!"

let deplace_sommet p1 p2 = 
  let n1, l1 = p1 in 
  let n2, l2 = p2 in 
  match (l1, l2) with 
  [], _ -> failwith (Printf.sprintf "Erreur")
  |(x::ll1, y::_) -> 
    if x > y then failwith (Printf.sprintf "Erreur")
    else ((n1, ll1),(n2, x::l2))
  |x::ll1, [] -> ((n1, ll1),(n2, [x]))

let joue l src dst autre =
  let p1 = choix_piquet l src in 
  let p2 = choix_piquet l dst in 
  let p3 = choix_piquet l autre in 
  let p1, p2 = deplace_sommet p1 p2 in [p1, p2, p3]

let rec gen_list n = 
  if n <= 0 then 
    []
  else
    gen_list (n - 1) @ [n]

let hanoi_list n =
  let rec hanoi_aux piquets dep mil arr n =
    if n > 0 then (
      let piquets = hanoi_aux piquets dep arr mil (n - 1) in
      let piquets = joue piquets dep arr mil in
      affiche_jeu piquets;
      Printf.printf "%s -> %s\n%!" dep arr;
      Unix.sleepf 0.1;
      hanoi_aux piquets mil dep arr (n - 1) )
    else piquets
  in
  let final =
    hanoi_aux [ ("dep", gen_list n);
                 ("mil", []);
                ("arr", []) ] "dep" "mil" "arr" n
  in
  affiche_jeu final
