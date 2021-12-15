(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n =
  if mod_b n [ 0; 0; 1 ] = [] then if n = [ 0; 0; 1 ] then true else false
  else
    let rec div_test i =
      if compare_b i n >= 0 then true
      else if mod_b n i = [] then false
      else div_test (add_b i [ 0; 0; 1 ])
    in
    div_test [ 0; 1; 1 ]

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  if p = [ 0; 0; 1 ] then true (* a refaire... *)
  else
    let rec power_mod_div = function
      | [] -> true
      | e :: l ->
          if e = p then power_mod_div l
          else if
            gcd_b p e = [ 0; 1 ]
            && mod_b (prime_mod_power e (diff_b p [ 0; 1 ]) p) p = [ 0; 1 ]
          then power_mod_div l
          else false
    in
    power_mod_div test_seq
