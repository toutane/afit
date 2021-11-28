(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  let rec add_to_list x =
    match x with
      x when x + 2 > n -> [x]
    | x -> x::(add_to_list (x + 2)) 
  in if n < 3 then [n]
     else 2::add_to_list 3
;;

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
 *)
let eratosthenes n =
  let supp_divisor n l =
    let rec list_browsing = function
      [] -> []
      | e::l -> if (modulo e n) = 0 then list_browsing l
                else e::(list_browsing l)
    in list_browsing l
  in let rec check_list = function
         [] -> []
       | e::l -> e::(check_list (supp_divisor e l))
     in check_list (init_eratosthenes n)
;;

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in
  let rec aux = function
      [] -> close_out oc
    | e::l -> Printf.fprintf oc "%n\n" e; aux l
  in aux li
;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = 
  write_list (eratosthenes n) file
;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let ic = open_in file in
  create_list ic
 ;;

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec build_list = function
    x when x = limit -> []
  | x -> if (isprime x) then
           let dp = x * 2 + 1 in
           if (isprime dp) then
             (x, dp)::(build_list (x + 1))
           else build_list (x + 1)
         else build_list (x + 1)
  in build_list 2
;;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec build_list = function
      x when x = limit -> []
    | x -> if (isprime x) then
             let twin = x + 2 in
             if (isprime twin) then
               (x, twin)::(build_list (x + 1))
             else build_list (x + 1)
           else build_list (x + 1)
  in build_list 2
;;
