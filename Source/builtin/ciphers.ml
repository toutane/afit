(** Ciphers
    Builtin integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let encrypt_cesar k m b =
  let rec browse_list = function
    | [] -> []
    | num :: l -> modulo (num + k) b :: browse_list l
  in
  browse_list m

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b =
  let rec browse_list = function
    | [] -> []
    | num :: l -> modulo (num - k) b :: browse_list l
  in
  browse_list m

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let n = p * q and phi = (p - 1) * (q - 1) and e = 3 in
  print_int phi;
  if gcd phi e != 1 then (
    let rec find_e x = if gcd phi x == 1 then x else find_e (x + 2) in
    let new_e = find_e (e + 2) in
    print_string "new e : ";
    print_int new_e;
    let d, _, _ = bezout new_e phi in
    ((n, new_e), (n, d)))
  else (
    print_string "e is good";
    let d, _, _ = bezout e phi in
    ((n, e), (n, d)))

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)

let decrypt_rsa m (n, d) = mod_power m d n

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let x = 2 + Random.int p in
  let isXPrimitive x =
    if mod_power x (p - 1) p = 1 then
      if mod_power x 2 p != 1 then
        if mod_power x ((p - 1) / 2) p != 1 then true else false
      else false
    else false
  in
  if isXPrimitive x then (x, p) else public_data_g p

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let x = p - 1 in
  (mod_power g x p, x)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)

let encrypt_g msg (g, p) kA =
  let rec get_random_k k =
    if modulo k (p - 1) = 0 then k else get_random_k (k + 1)
  in
  let k = get_random_k 2 in
  let a = mod_power g k p and b = modulo (power kA k * msg) p in
  (a, b)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = modulo (quot msgB (power msgA a)) p
