let bissextile a = 
  if a mod 4 = 0 || (a mod 100 = 0 && a mod 400 = 0) then true
  else false

  let jour_mois m a =
    if m = 2 then
      if bissextile a then 29 else 28
    else
      let m = if m >= 8 then m - 7 else m in
      30 + (m mod 2)

let rec jour_date j m a = 
  if m = 1 then j
  else 
    (jour_mois (m-1) a) + jour_date j (m-1) a

let () = Printf.printf "jour_date 31 12 2020 = %d\n" (jour_date 31 12 2020)
let () = Printf.printf "jour_date 31 12 2019 = %d\n" (jour_date 31 12 2019)

(*Test #2 *)
let x = jour_mois 1 2023
let y = jour_mois 2 2023
let z = jour_mois 6 2023
let e = jour_mois 9 2023
let q = jour_mois 2 2024
let w = jour_mois 6 2024
let r = jour_mois 9 2024

let() = Printf.printf "Test #2\n"
let() = Printf.printf "L'annee 2023 et le 1 mois: %d\n" x
let() = Printf.printf "L'annee 2023 est le 2 mois: %d\n" y
let() = Printf.printf "L'annee 2023 est le 6 mois: %d\n" z
let() = Printf.printf "L'annee 2023 est le 9 mois %d\n" e
let() = Printf.printf "L'annee 2024 est le 2 mois %d\n" q
let() = Printf.printf "L'annee 2024 est le 6 mois %d\n" w
let() = Printf.printf "L'annee 2024 est le 9 mois %d\n" w
let() = Printf.printf "\n"