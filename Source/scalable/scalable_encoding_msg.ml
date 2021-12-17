(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let rec len = String.length str in
  let rec addition i =
    if i > len - 1 then []
    else
      match from_int (Char.code str.[i]) with
      | [] -> []
      | _ :: l -> addition (i + 1) @ l
  in
  0 :: addition 0

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)

let decode msg bits =
  let base = power [ 0; 0; 1 ] (from_int bits) in
  let rec dechiffre nmsg =
    if compare_b nmsg [] = 0 then ""
    else
      dechiffre (quot_b nmsg base)
      ^ Char.escaped (char_of_int (to_int (mod_b nmsg base)))
  in
  dechiffre msg
