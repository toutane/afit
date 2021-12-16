(** Ciphers
    Big integers based ciphers.
*)

open Z
open Z_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let n = p * q and phi = (p - one) * (q - one) and e = of_int 3 in
  if equal (gcd phi e) one then
    let _, d, _ = gcdext e phi in
    ((n, e), (n, d))
  else
    let rec find_e x =
      if equal (gcd phi x) one then x else find_e (x + of_int 2)
    in
    let new_e = find_e (e + of_int 2) in
    let _, d, _ = gcdext new_e phi in
    ((n, new_e), (n, d))

(** Encryption using RSA cryptosystem.
    @param m big integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n

(** Decryption using RSA cryptosystem.
    @param m big integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n, d) = mod_power m d n

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let x = of_int 2 + of_int (Random.int (to_int p)) in
  let isXPrimitive x =
    if equal (mod_power x (p - one) p) one then
      if not (equal (mod_power x (of_int 2) p) one) then
        if not (equal (mod_power x (div (p - one) (of_int 2)) p) one) then true
        else false
      else false
    else false
  in
  if isXPrimitive x then (x, p) else public_data_g p

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let x = p - one in
  (mod_power g x p, x)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let rec get_random_k k =
    if equal (erem k (p - one)) zero then k else get_random_k (k - one)
  in
  let k = get_random_k p in
  let a = mod_power g k p and b = erem (pow kA (to_int k) * msg) p in
  (a, b)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = erem (ediv msgB (pow msgA (to_int a))) p
