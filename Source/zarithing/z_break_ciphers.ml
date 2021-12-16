(** Factoring big integers into primes *)

open Z

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let n, e = key in
  let rec getP x = if equal (erem n x) zero then x else getP (x - one) in
  let p = getP (n - one) in
  let q = div n p in
  (q, p)
