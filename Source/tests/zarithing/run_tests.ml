open Alcotest
open Z
open Test_tools
open Test_z_power
open Test_z_test_primes
open Test_z_generate_primes
open Test_z_encoding_msg
open Test_z_ciphers
open Test_z_break_ciphers

let z_test_suite =
  [
    ("z_power", power_set);
    ("z_test_primes", test_primes_set);
    ("z_generate_primes", generate_primes_set);
    ("z_ciphers", ciphers_set);
    ("z_break_ciphers", break_ciphers_set);
    ("z_encoding_msg", encoding_msg_set);
  ]

let () = run_to_xml "trace_zarithing_1.xml" [ z_test_suite ]
