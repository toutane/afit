(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)

let rec gcd_b bA bB = if bB = [] then bA else gcd_b bB (mod_b bA bB)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  let rec loop r1 u1 v1 r2 u2 v2 =
    if r2 = [] then (u1, v1, r1)
    else
      let q = quot_b r1 r2 in
      loop r2 u2 v2
        (diff_b r1 (mult_b q r2))
        (diff_b u1 (mult_b q u2))
        (diff_b v1 (mult_b q v2))
  in
  loop bA [ 0; 1 ] [] bB [] [ 0; 1 ]
