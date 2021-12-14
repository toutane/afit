(** Basic arithmetics with builtin integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b = if b = 0 then a else gcd b (modulo a b)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let rec loop r1 u1 v1 r2 u2 v2 =
    if r2 = 0 then (u1, v1, r1)
    else
      let q = quot r1 r2 in
      loop r2 u2 v2 (r1 - (q * r2)) (u1 - (q * u2)) (v1 - (q * v2))
  in
  loop a 1 0 b 0 1
