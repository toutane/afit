(** Test suites for big int encoding_msg ml file using aclotest. *)

open Alcotest
open Z
open Z_encoding_msg

let sprintf = Printf.sprintf

let zt = testable pp_print Z.equal

let encode_tests () =
  let cases = [ (("Bashar", 7), of_int 2294023860466) ]
  and do_check ((str, bits), expected) =
    check zt
      (sprintf "encode: \"%s\" on %i bits" str bits)
      expected (encode str bits)
  in
  List.iter do_check cases

let decode_tests () =
  let cases = [ ((of_int 2294023860466, 7), "Bashar") ]
  and do_check ((msg, bits), expected) =
    check string
      (sprintf "decode: %s on %i bits" (to_string msg) bits)
      expected (decode msg bits)
  in
  List.iter do_check cases

let encoding_msg_set =
  [
    ("Encode with bitarrays function", `Quick, encode_tests);
    ("Decode with bitarrays function", `Quick, decode_tests);
  ]
