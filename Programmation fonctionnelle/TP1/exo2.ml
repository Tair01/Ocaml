open Sys
let file = Sys.argv.(1)
let() = 
  if Array.length Sys.argv = 0 then 
    Printf.printf "Pas assez d'argument"
  else 
    if Sys.file_exists file then 
      if Sys.is_directory file then 
        Printf.printf "Le repertoire existe\n"
      else
        Printf.printf "Le fichier existe\n"
    else
      Printf.printf "Le fichier n'existe pas\n"
