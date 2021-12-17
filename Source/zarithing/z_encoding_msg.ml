(** Encoding Strings *)

open Z
open Z_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let base = pow (of_int 2) bits and length = String.length str in
  let rec chiffre i decalage =
    if equal i minus_one then zero
    else
      (of_int (Char.code str.[to_int i]) * decalage)
      + chiffre (i - one) (base * decalage)
  in
  chiffre (of_int length - one) one

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)

let decode msg bits =
  let base = pow (of_int 2) bits in
  let rec dechiffre nmsg =
    if equal nmsg zero then ""
    else
      dechiffre (ediv nmsg base)
      ^ Char.escaped (char_of_int (to_int (erem nmsg base)))
  in
  dechiffre msg
