(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)
let generate_keys_rsa p q =
  let n = mult_b p q
  and phi = mult_b (diff_b p [ 0; 1 ]) (diff_b q [ 0; 1 ])
  and e = [ 0; 1; 0; 1 ] in
  if compare_b (gcd_b phi e) [ 0; 1 ] != 0 then
    let rec find_e x =
      if compare_b (gcd_b phi x) [ 0; 1 ] == 0 then x
      else find_e (add_b x [ 0; 0; 1 ])
    in
    let new_e = find_e (add_b e [ 0; 0; 1 ]) in
    let d, _, _ = bezout_b new_e phi in
    ((n, new_e), (n, d))
  else
    let d, _, _ = bezout_b e phi in
    ((n, e), (n, d))

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n, d) = mod_power m d n

(********** ElGamal Cipher **********)

let rec public_data_g p =
  let x = add_b [ 0; 0; 1 ] (from_int (Random.int (to_int p))) in
  let isXPrimitive x =
    if compare_b (mod_power x (diff_b p [ 0; 1 ]) p) [ 0; 1 ] = 0 then
      if compare_b (mod_power x [ 0; 0; 1 ] p) [ 0; 1 ] != 0 then
        if mod_power x (quot_b (diff_b p [ 0; 1 ]) [ 0; 0; 1 ]) p != [ 0; 1 ]
        then true
        else false
      else false
    else false
  in
  if isXPrimitive x then (x, p) else public_data_g p

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let x = diff_b p [ 0; 1 ] in
  (mod_power g x p, x)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let rec get_random_k k =
    print_string "searching for k = ";
    print_b k;
    print_newline ();
    if to_int (mod_b k (diff_b p [ 0; 1 ])) = 0 then k
    else get_random_k (diff_b k [ 0; 1 ])
  in
  let k = get_random_k p in
  print_string "found k = ";
  print_newline ();
  print_b k;
  print_newline ();
  let a = mod_power g k p and b = mod_b (mult_b (power kA k) msg) p in
  (a, b)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = mod_b (quot_b msgB (power msgA a)) p
