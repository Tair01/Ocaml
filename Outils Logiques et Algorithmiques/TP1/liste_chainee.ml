let rec est_triee l = 
  match l with 
  |[] -> true
  | [_] -> true
  |x1 :: x2 :: l -> 
    if x1 <= x2 then est_triee (x2 :: l)
    else false   

let rec insere x l = 
  match l with 
  [] -> [x]
  |e1 :: ll -> if x < e1 then x :: l
              else  e1 :: insere x ll 
  
let rec tri_insertion l = 
  match l with 
  [] -> []
  | x :: ll -> insere x (tri_insertion ll)

let rec doublons l = 
  match l with 
  | [] | [_] -> l
  | x1 :: x2 :: ll -> if x1 = x2 then doublons (x1 :: ll)
                      else x1 :: doublons(x2 :: ll)

let rec take n l = 
  if n=0 then []
  else (List.hd l) :: (take(n -1) (List.tl l))

let rec chop n l = 
  if n = 0 then l
  else chop (n-1) (List.tl l)

let separe l = 
  let len = List.length l in 
  let half_len = len / 2 in 
  let l1 = take half_len l in 
  let l2 = chop half_len l in 
  (l1,l2)

let rec fusion l1 l2 = 
  match l1, l2 with
  | [], l | l, [] -> l
  | x1 :: ll1, x2 :: ll2 -> if x1 <= x2 then x1 :: (fusion ll1 ll2)
                            else x2 :: (fusion l1 ll2)

let rec tri_fusion l = 
  match l with 
  | [] | [_] -> l
  | _-> 
    let (l1, l2) = separe l in 
    let l1_triee = tri_fusion l1 in 
    let l2_triee = tri_fusion l2 in 
    fusion l1_triee l2_triee 