open Unix 
let gen_list f n =
  let rec loop i acc = 
    if i < 0 then acc 
    else loop (i - 1) ((f i ) :: acc)
  in 
  loop (n - 1)[]


let int_list n = gen_list (fun x -> x) n

let rand_list n = gen_list(fun x -> Random.int n) n

let rec time f x = 
  Gc.compact();
  let t0 = Unix.gettimeofday () in 
  let r = f x in 
  let t1 = Unix.gettimeofday () in 
  (r, (t1 -. t0) *. 1000.)

let comp a x = a - x

let insert_sort comp l = 
  let rec ins l x = 
    match l with 
    [] -> [x]
    | e1 :: ll -> 
      if comp x e1  <= 0 then x :: e1 :: ll 
      else e1 :: ins ll x 
  in
  List.fold_left ins [] l  

  let test_sort f n sizes =
    Random.init 42;
    Printf.printf "%s\n" n;
    let display l tname =
      Printf.printf "%d (%s) %f\n" (List.length l) tname;
      ignore (time f l)
    in
    let sorted = List.map (fun x -> int_list x) sizes in
    let random = List.map (fun x -> rand_list x) sizes in
    List.iter (fun l -> display (insert_sort compare l) "trié") sorted;
    List.iter (fun l -> display (f l) "random") random;
    print_newline ()

let t = test_sort ( insert_sort compare) "insert_sort" [0;1;10;20;50;100] 
