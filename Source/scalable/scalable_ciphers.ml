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
let encrypt_rsa m (n, e) = []

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n, d) = []

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p = ([], [])

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = ([], [])

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = ([], [])

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = []
