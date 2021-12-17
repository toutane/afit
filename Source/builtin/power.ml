(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  let rec multi i = if i = 0 then 1 else x * multi (i - 1) in
  multi n

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  let rec multi_f x i =
    if i = 0 then 1
    else if modulo i 2 = 0 then multi_f (x * x) (quot i 2)
    else x * multi_f (x * x) (quot (i - 1) 2)
  in
  multi_f x n

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)

let mod_power x n m =
  let rec multiply b e =
    if e = 0 then 1
    else if e = 1 then modulo b m
    else
      let t = multiply b (e / 2) in
      let k = modulo (t * t) m in
      if e mod 2 = 0 then k else modulo (k * modulo b m) m
  in
  multiply x n

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if n < 0 then invalid_arg "prime_mod_power: n should be natural"
  else if n = 0 then 1
  else if modulo x p <> 0 then mod_power x (modulo n (p - 1)) p
  else mod_power x n p
