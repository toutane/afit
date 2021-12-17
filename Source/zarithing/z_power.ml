(** Power function implementations for big integers *)

open Z

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m = powm x n m

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
*)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if lt n zero then invalid_arg "prime_mod_power: n should be natural"
  else if equal n zero then one
  else if not (equal (erem x p) zero) then mod_power x (erem n (p - one)) p
  else mod_power x n p
