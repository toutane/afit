(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let base = power 2 bits and length = String.length str in
  let rec chiffre i decalage =
    if i = -1 then 0
    else (Char.code str.[i] * decalage) + chiffre (i - 1) (base * decalage)
  in
  chiffre (length - 1) 1

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)

let decode msg bits =
  let base = power 2 bits in
  let rec dechiffre nmsg =
    if nmsg = 0 then ""
    else
      dechiffre (quot nmsg base) ^ Char.escaped (char_of_int (modulo nmsg base))
  in
  dechiffre msg
