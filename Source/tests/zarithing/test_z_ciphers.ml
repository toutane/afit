(** Test suites for builtin cifers ml file using alcotest. *)

open Alcotest
open Z
open Test_tools
open Z_ciphers

let sprintf = Printf.sprintf

let zt = testable pp_print Z.equal

let generate_keys_rsa_tests () =
  let cases = [ ((of_int 9967, of_int 9973), true) ]
  and do_check ((p, q), expected) =
    (* Here, `expected` is a boolean telling if the generated `e` and `d`
     * are the inverse of each other modulo `n` *)
    let (_, e), (n, d) = generate_keys_rsa p q
    and phin = (p - one) * (q - one)
    and is_inverse x y n = equal (erem (erem x n * erem y n) n) one in
    check bool
      (sprintf
         "generate_rsa_keys: with %s and %s (we check if e is the inverse of d \
          modulo n)"
         (to_string p) (to_string q))
      true (is_inverse e d phin)
  in
  List.iter do_check cases

let encrypt_rsa_tests () =
  let cases =
    [ ((of_int 281237, (of_int 99400891, of_int 36199003)), of_int 70133953) ]
  and do_check ((m, (n, e)), expected) =
    check zt
      (sprintf "encrypt_rsa: %s with n=%s and e=%s" (to_string m) (to_string n)
         (to_string e))
      expected
      (encrypt_rsa m (n, e))
  in
  List.iter do_check cases

let decrypt_rsa_tests () =
  let cases =
    [ ((of_int 70133953, (of_int 99400891, of_int 30869683)), of_int 281237) ]
  and do_check ((m, (n, e)), expected) =
    check zt
      (sprintf "decrypt_rsa: %s with n=%s and e=%s" (to_string m) (to_string n)
         (to_string e))
      expected
      (decrypt_rsa m (n, e))
  in
  List.iter do_check cases

let el_gamal_tests () =
  let cases = [ (of_int 100000007, of_int 42) ]
  and do_check (p, msg) =
    (* No need for an expected value, we encrypt and decrypt a message so
     * the expected result is the original message *)
    let g, p = public_data_g p in
    let pub, priv = generate_keys_g (g, p) in
    let g_k, xA_k = encrypt_g msg (g, p) pub in
    check zt
      (sprintf
         "el_gamal functions: keys generated with g=%s, message %s is \
          encrypted and then decrypted, we check if the result is equal to the \
          original message"
         (to_string p) (to_string msg))
      msg
      (decrypt_g (g_k, xA_k) priv (g, p))
  in
  List.iter do_check cases

let ciphers_set =
  [
    ("Zarithing generate RSA keys function", `Quick, generate_keys_rsa_tests);
    ( "Zarithing RSA encryption with bitarrays function",
      `Quick,
      encrypt_rsa_tests );
    ( "Zarithing RSA decryption with bitarrays function",
      `Quick,
      decrypt_rsa_tests );
    ( "Zarithing El Gamal cryptosystem with bitarrays functions",
      `Quick,
      el_gamal_tests );
  ]
