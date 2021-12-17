(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n =
  if modulo n 2 = 0 then if n = 2 then true else false
  else
    let rec div_test i =
      if i >= n then true else if modulo n i = 0 then false else div_test (i + 2)
    in
    div_test 3

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
  if p = 2 then true (* a refaire... *)
  else
    let rec power_mod_div = function
      | [] -> true
      | e :: l ->
          if e = p then power_mod_div l
          else if gcd p e = 1 && modulo (prime_mod_power e (p - 1) p) p = 1 then
            power_mod_div l
          else false
    in
    power_mod_div test_seq
