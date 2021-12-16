(** Testing for primality *)

open Z
open Z_power

(** Deterministic primality test
    @param n a big integer bigger or equal to 2.
 *)
let is_prime n =
  if equal (erem n (of_int 2)) zero then
    if equal n (of_int 2) then true else false
  else
    let rec div_test i =
      if geq i n then true
      else if equal (erem n i) zero then false
      else div_test (i + of_int 2)
    in
    div_test (of_int 3)
