(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let n, e = key in
  let rec getP x = if modulo n x = 0 then x else getP (x - 1) in
  let p = getP (n - 1) in
  let q = n / p in
  (q, p)
