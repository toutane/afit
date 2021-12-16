(** Test suites for big int generate primes ml file using alcotest. *)

open Alcotest
open Z
module P = Z_test_primes
open Z_generate_primes

let sprintf = Printf.sprintf

let zt = testable pp_print Z.equal

let init_eratosthenes_tests () =
  let cases =
    [
      (of_int 2, [ of_int 2 ]);
      (of_int 3, [ of_int 2; of_int 3 ]);
      (of_int 6, [ of_int 2; of_int 3; of_int 5 ]);
    ]
  and do_check (n, expected) =
    check (list zt)
      (sprintf "init_eratosthenes: %s" (to_string n))
      expected (init_eratosthenes n)
  in
  List.iter do_check cases

let eratosthenes_tests () =
  let cases =
    [
      (of_int 2, [ of_int 2 ]);
      (of_int 3, [ of_int 2; of_int 3 ]);
      (of_int 6, [ of_int 2; of_int 3; of_int 5 ]);
      ( of_int 25,
        [
          of_int 2;
          of_int 3;
          of_int 5;
          of_int 7;
          of_int 11;
          of_int 13;
          of_int 17;
          of_int 19;
          of_int 23;
        ] );
    ]
  and do_check (n, expected) =
    check (list zt)
      (sprintf "eratosthenes: %s" (to_string n))
      expected (eratosthenes n)
  in
  List.iter do_check cases

let double_primes_tests () =
  let cases =
    [
      ( (of_int 20, P.is_prime),
        [
          (of_int 2, of_int 5);
          (of_int 3, of_int 7);
          (of_int 5, of_int 11);
          (of_int 11, of_int 23);
        ] );
    ]
  and do_check ((n, isprime), expected) =
    check
      (list (pair zt zt))
      (sprintf "double_primes: %s" (to_string n))
      expected (double_primes n isprime)
  in
  List.iter do_check cases

let generate_primes_set =
  [
    ( "Zarithing eratosthenes initialisation with bitarrays function",
      `Quick,
      init_eratosthenes_tests );
    ( "Zarithing eratosthenes with bitarrays function",
      `Quick,
      eratosthenes_tests );
    ( "Zarithing double primes with bitarrays function",
      `Quick,
      double_primes_tests );
  ]
