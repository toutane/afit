(** Test suites for z_power ml file using alcotest. *)

open Alcotest
open Z
open Z_power

let sprintf = Printf.sprintf

let zt = testable pp_print Z.equal

let mod_power_tests () =
  let cases =
    [
      ((of_int (-1), of_int 12, of_int 10), of_int 1);
      ((of_int (-1), of_int 11, of_int 11), of_int 10);
      ((of_int 0, of_int 2, of_int 3), of_int 0);
      ((of_int 3, of_int 1, of_int 3), of_int 0);
      ((of_int 5, of_int 0, of_int 2), of_int 1);
      ((of_int (-2), of_int 2, of_int 5), of_int 4);
      ((of_int (-2), of_int 3, of_int 9), of_int 1);
      ((of_int 2, of_int 5, of_int 17), of_int 15);
      ((of_int 3, of_int 3, of_int 17), of_int 10);
    ]
  and do_check ((x, n, m), expected) =
    check zt
      (sprintf "%s^%s modulo %s" (to_string x) (to_string n) (to_string m))
      expected (mod_power x n m)
  in
  List.iter do_check cases

let prime_mod_power_tests () =
  let cases =
    [
      ((of_int (-1), of_int 12, of_int 7), of_int 1);
      ((of_int (-1), of_int 11, of_int 11), of_int 10);
      ((of_int 0, of_int 2, of_int 3), of_int 0);
      ((of_int 3, of_int 1, of_int 3), of_int 0);
      ((of_int 5, of_int 0, of_int 2), of_int 1);
      ((of_int (-2), of_int 2, of_int 5), of_int 4);
      ((of_int (-2), of_int 3, of_int 5), of_int 2);
      ((of_int 2, of_int 5, of_int 17), of_int 15);
      ((of_int 3, of_int 3, of_int 17), of_int 10);
    ]
  and do_check ((x, n, p), expected) =
    check zt
      (sprintf "%s^%s modulo %s" (to_string x) (to_string n) (to_string p))
      expected (prime_mod_power x n p)
  in
  List.iter do_check cases

let power_set =
  [
    ("Zarithing modular power function", `Quick, mod_power_tests);
    ( "Zarithing modular power with prime modulo power function",
      `Quick,
      prime_mod_power_tests );
  ]
