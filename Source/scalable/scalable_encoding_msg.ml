(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power
open String
open Char

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let rec len = length str in
  let rec addition i =
    if i > len - 1 then []
    else
      match from_int (code str.[i]) with
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
  let rec arrToString = function
    | [] -> ""
    | e :: l -> arrToString l ^ string_of_int e
  in
  let rec subStr str a len =
    if a > len - 1 then "" else make 1 str.[a] ^ subStr str (a + 1) len
  in
  let bin = arrToString msg in
  let rec breakBin msg i pos =
    if i > 0 then
      subStr bin pos (pos + bits) :: breakBin bin (i - 1) (pos + bits)
    else []
  in
  let rec bin_to_dec i j str =
    if i > 0 then
      (bin_to_dec (i - 1) (j - 1) str * 2) + (int_of_char str.[j] - 48)
    else 0
  in
  let rec bin_to_char = function
    | [] -> ""
    | e :: l ->
        escaped (chr (bin_to_dec (length e) (length e - 1) e)) ^ bin_to_char l
  in
  bin_to_char (breakBin bin (length bin / bits) 0)
