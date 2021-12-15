(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  let rec multi i =
    if i = [] then [ 0; 1 ] else mult_b x (multi (diff_b i [ 0; 1 ]))
  in
  multi n

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  let rec multi_f x i =
    if i = [] then [ 0; 1 ]
    else if mod_b i [ 0; 0; 1 ] = [] then
      multi_f (mult_b x x) (quot_b i [ 0; 0; 1 ])
    else
      mult_b x (multi_f (mult_b x x) (quot_b (diff_b i [ 0; 1 ]) [ 0; 0; 1 ]))
  in
  multi_f x n

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m =
  let rec multiply b e =
    if e = [] then [ 0; 1 ]
    else if e = [ 0; 1 ] then mod_b b m
    else
      let t = multiply b (quot_b e [ 0; 0; 1 ]) in
      let k = mod_b (mult_b t t) m in
      if mod_b e [ 0; 0; 1 ] = [] then k else mod_b (mult_b k (mod_b b m)) m
  in
  multiply x n

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  if x = [] then []
  else
    let rec multi_mf x i =
      if i = [] || i = diff_b p [ 0; 1 ] then [ 0; 1 ]
      else
        let k =
          if mod_b i [ 0; 0; 1 ] = [] then
            multi_mf (mult_b x x) (quot_b i [ 0; 0; 1 ])
          else
            mult_b x
              (multi_mf (mult_b x x) (quot_b (diff_b i [ 0; 1 ]) [ 0; 0; 1 ]))
        in
        mod_b k p
    in
    multi_mf x n
