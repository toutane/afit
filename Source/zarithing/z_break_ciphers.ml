(** Factoring big integers into primes *)

open Z

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)

let break key =
  let n, e = key in
  let rec find_p i =
    if equal (erem n i) zero then (i, ediv n i) else find_p (i + one)
  in
  find_p (of_int 2)
