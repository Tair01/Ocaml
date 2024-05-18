let rec pgcd a b = 
  if b == 0 then a
  else pgcd b(a mod b)

type frac = {num : int; denom : int}
let sign i = 
  match i with 
  |i when i > 0 -> 1
  |i when i < 0 -> -1
  |_ -> 0

let simp f = 
  let num, denom = 
  if sign f.num * sign f.denom >= 0 then 
    abs f.num, abs f.denom
  else
    -(abs f.num), (abs f.denom)
  in 
  let p = pgcd(abs num) denom in 
  {num = num / p; denom = denom  / p}

let frac a b = simp {num = a; denom = b}

let add_frac f1 f2 = 
  frac(f1.num * f2.denom + f2.num * f1.denom)(f1.denom * f2.denom)

let neg_frac f = {num = -f.num; denom = f.denom}

let sub_frac f1 f2 = 
  frac(f1.num * f2.denom - f2.num * f1.denom)(f1.denom * f2.denom)

let mul_frac f1 f2 = 
  frac(f1.num * f2.num)(f1.denom * f2.denom)

let inv_frac f = frac f.denom f.num

let div_frac f1 f2 = 
  mul_frac f1 (inv_frac f2)

let string_of_frac f = Printf.sprintf "%d/%d" f.num f.denom

let float_of_frac f = (float f.num) /. (float f.denom)

type num = Int of int | Float of float |Frac of frac

let string_of_num n = 
  match n with 
  |Int n -> string_of_int n
  |Float n -> string_of_float n
  |Frac n -> string_of_frac n


  let exec_op n1 n2 op_i op_fr op_fl =
    match n1 , n2 with
    | Float fl1 , Float fl2 -> Float (op_fl fl1 fl2)
    | Float fl1 , Frac fr2 -> Float (op_fl fl1 (float_of_frac fr2))
    | Frac fr1, Frac fr2 -> Frac (op_fr fr1 fr2)
    | Frac fr1, Float fl2 -> Float(op_fl (float_of_frac fr1) fl2)
    | Int it1, Int it2 -> Int(op_i it1 it2)
    | Int it1, Float fl2 -> Int(op_i it1 (int_of_float fl2))
    |_ -> failwith "Operation non suportee"

let add_num n1 n2 = 
  let add_int x y = x + y in 
  let add_frac fr1 fr2 =  add_frac fr1 fr2 in
  let add_float f1 f2 = f1 +. f2 in 
  exec_op n1 n2 add_int add_frac add_float
  
let sub_num n1 n2 = 
  let sub_int x y = x - y in 
  let sub_frac fr1 fr2 = sub_frac fr1 fr2 in 
  let sub_float f1 f2 = f1 -. f2 in 
  exec_op n1 n2 sub_int sub_frac sub_float

let mul_num n1 n2 = 
  let mul_int x y = x * y in 
  let mul_frac fr1 fr2 = mul_frac fr1 fr2 in
  let mul_float f1 f2 = f1 *. f2 in 
  exec_op n1 n2 mul_int mul_frac mul_float

let div_num n1 n2 = 
  let div_int x y = x / y in 
  let div_frac fr1 fr2 = div_frac fr1 fr2 in 
  let div_float f1 f2 = f1 /. f2 in 
  exec_op n1 n2 div_int div_frac div_float

let rec pow n k acc = 
  if n == 0 then acc 
  else pow n (k - 1)(acc * n)



