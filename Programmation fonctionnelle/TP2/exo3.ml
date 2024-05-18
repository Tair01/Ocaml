let rec hanoi_aux dep mil arr n=
  if n > 0 then begin
    hanoi_aux dep arr mil (n-1);
    Printf.printf "%s -> %s\n" dep arr;
    hanoi_aux mil dep arr (n-1);
  end

let hanoi n  = hanoi_aux "A" "B" "C" 5

let t0 = Sys.time()
let () = hanoi 10 
let t1 = Sys.time () 
let () = Printf.printf "Temps écoulé : %f ms\n" ((t1 -. t0) *. 1000.)