(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power
open String
open Char

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = length str in
  let rec intList i =
    if i > len - 1 then [] else code str.[i] :: intList (i + 1)
  in
  let rec dec_to_bin n p =
    if n <> 0 || p > 0 then
      dec_to_bin (quot n 2) (p - 1) ^ string_of_int (modulo n 2)
    else ""
  in
  let rec intListToStr = function
    | [] -> ""
    | e :: l -> dec_to_bin e bits ^ intListToStr l
  in
  let rec bin_to_dec i j str =
    if i > 0 then
      (bin_to_dec (i - 1) (j - 1) str * 2) + (int_of_char str.[j] - 48)
    else 0
  in
  let bin = intListToStr (intList 0) in
  bin_to_dec (length bin) (length bin - 1) bin

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let rec dec_to_bin n =
    if n <> 0 then dec_to_bin (quot n 2) ^ string_of_int (modulo n 2) else ""
  in
  let rec subStr str a len =
    if a > len - 1 then "" else make 1 str.[a] ^ subStr str (a + 1) len
  in
  let rec breakBin bin i pos =
    if i > 0 then
      subStr bin pos (pos + bits) :: breakBin bin (i - 1) (pos + bits)
    else []
  in
  let bin = dec_to_bin msg in
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
