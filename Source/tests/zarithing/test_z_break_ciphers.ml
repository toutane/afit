(** Test suites for big int break_cifers ml file using alcotest. *)

open Alcotest
open Z
open Z_break_ciphers

let sprintf = Printf.sprintf

let zt = testable pp_print Z.equal

(* Only tests for RSA for now. *)

let break_tests () =
  let cases =
    [ ((of_int 99400891, of_int 36199003), (of_int 9967, of_int 9973)) ]
  and do_check ((n, e), expected) =
    check (pair zt zt)
      (sprintf "break: n=%s and e=%s" (to_string n) (to_string e))
      expected
      (break (n, e))
  in
  List.iter do_check cases

let break_ciphers_set =
  [
    ("Zarithing RSA key breaking with bitarrays function", `Quick, break_tests);
  ]
