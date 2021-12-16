(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let n, e = key in
  let sqrt x =
    let res = [] and start = [ 0; 1 ] and en = quot_b x [ 0; 0; 1 ] in
    let rec loop s e res =
      if compare_b s e <= 0 then
        let mid = quot_b (add_b s e) [ 0; 0; 1 ] in
        let r = mult_b mid mid in
        match compare_b r x with
        | 0 -> mid
        | -1 ->
            let s2 = add_b mid [ 0; 1 ] and res2 = mid in
            loop s2 e res2
        | _ ->
            let e2 = diff_b mid [ 0; 1 ] in
            loop s e2 res
      else res
    in
    loop start en res
    (*if compare_b (mult_b i i) j = 0 then i else sqrt (diff_b i [ 0; 1 ]) j*)
  in
  let rec getP x = if mod_b n x = [] then x else getP (diff_b x [ 0; 1 ]) in
  (*let p = getP (diff_b n [ 0; 1 ]) in*)
  let p = getP (sqrt n) in
  let q = quot_b n p in
  (p, q)
