(** Encoding Strings *)

open Z
open Z_power
open String
open Char

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = of_int (length str) in
  let rec intList i =
    if gt i (len - one) then []
    else of_int (code str.[to_int i]) :: intList (i + one)
  in
  let rec dec_to_bin n p =
    if (not (Z.equal n zero)) || gt p zero then
      dec_to_bin (ediv n (of_int 2)) (p - one) ^ to_string (erem n (of_int 2))
    else ""
  in
  let rec intListToStr = function
    | [] -> ""
    | e :: l -> dec_to_bin e (of_int bits) ^ intListToStr l
  in
  let rec bin_to_dec i j str =
    if gt i zero then
      (bin_to_dec (i - one) (j - one) str * of_int 2)
      + of_int (int_of_char str.[to_int j])
      - of_int 48
    else zero
  in
  let bin = intListToStr (intList zero) in
  bin_to_dec (of_int (length bin)) (of_int (length bin) - one) bin

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let rec dec_to_bin n =
    if not (Z.equal n zero) then
      dec_to_bin (ediv n (of_int 2)) ^ to_string (erem n (of_int 2))
    else ""
  in
  let rec subStr str a len =
    if gt a (len - one) then ""
    else make 1 str.[to_int a] ^ subStr str (a + one) len
  in
  let rec breakBin bin i pos =
    if gt i zero then
      subStr bin pos (pos + of_int bits)
      :: breakBin bin (i - one) (pos + of_int bits)
    else []
  in
  let bin = dec_to_bin msg in
  let rec bin_to_dec i j str =
    if gt i zero then
      (bin_to_dec (i - one) (j - one) str * of_int 2)
      + (of_int (int_of_char str.[to_int j]) - of_int 48)
    else zero
  in
  let rec bin_to_char = function
    | [] -> ""
    | e :: l ->
        escaped
          (chr
             (to_int
                (bin_to_dec (of_int (length e)) (of_int (length e) - one) e)))
        ^ bin_to_char l
  in
  bin_to_char (breakBin bin (ediv (of_int (length bin)) (of_int bits)) zero)
