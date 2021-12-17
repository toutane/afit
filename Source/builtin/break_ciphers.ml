(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let n, e = key in
  let rec find_p i = if modulo n i = 0 then (i, quot n i) else find_p (i + 1) in
  find_p 2
