type movie = {
  id:int;
  title:string;
  year:int;
  runtime:int;
  rank:int
}

type res = Movie of movie | Invalid |  Eof

let input_movie in_c = 
  try
    let s = input_line in_c in 
    match String.split_on_char ';' s with 
    [s_id; title; s_year; s_runtime; s_rank;] -> 
      Movie({
        id = int_of_string s_id;
        title = title;
        year = int_of_string s_year;
        runtime = int_of_string s_runtime;
        rank = int_of_string s_rank;
      })
    |_-> Invalid
  with
    End_of_file -> Eof
  | _ -> Invalid

let load_movies f = 
  let in_c = open_in f in 
  let rec loop in_c acc = 
    match input_movie in_c with 
    |Eof -> acc 
    |Invalid -> loop in_c acc 
    |Movie m -> loop in_c (m::acc)
  in 
    let res = loop in_c [] in 
    close_in in_c;
    res

let movies = load_movies "movies.csv"

let pr_movie m = 
  Printf.printf "{id = %d; title = %s; year = %d; runtime = %d; rank = %d; }" 
  m.id m.title m.year m.runtime m.rank

let pr_movies m = List.iter(fun x -> pr_movie x; Printf.printf "\n") m

let moviesTop10 m = List.filter(fun x -> x.rank <= 10) m

let movies1980 m = List.filter(fun x -> (x.year > 1980)&& (x.year < 1990)) m

let movie_titles m = List.map(fun x-> x.title) m

let max_id m = List.fold_left(fun acc x -> if x.id > acc then x.id else acc) 0 m

let min_id m = List.fold_left(fun acc x -> if x.id < acc then x.id else acc )max_int m

let average_runtime m = 
  let(t_x, t_count) = 
  List.fold_left(fun (x,count) m1 -> (x + m1.runtime, count + 1))(0,0) m
  in
  float_of_int t_x /. float_of_int t_count 

let average_by_year m = 
  let t = List.sort(fun a b -> a.year - b.year) m in 
  let rec loop acc movie_list = 
    match movie_list with 
    [] -> acc 
    |hd::tl -> 
      match acc with 
      |[] -> loop [(hd.year, [hd])] tl
      |(y, lst)::rest -> 
        if hd.year = y then 
          let new_acc = (y, hd :: lst) :: rest in
          loop new_acc tl
        else
          let new_acc = (hd.year, [hd]) :: acc in
          loop new_acc tl
    in
    let grouped_by_year = loop [] t in

    List.map (fun (y, lst) -> (y, average_runtime lst)) grouped_by_year



