(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
  let rec add_to_list x =
    match x with
    | x when compare_b (add_b x [ 0; 0; 1 ]) n >= 0 -> [ x ]
    | x -> x :: add_to_list (add_b x [ 0; 0; 1 ])
  in
  if compare_b n [ 0; 1; 1 ] < 0 then [ n ]
  else [ 0; 0; 1 ] :: add_to_list [ 0; 1; 1 ]

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  let supp_divisor n l =
    let rec list_browsing = function
      | [] -> []
      | e :: l ->
          if mod_b e n = [] then list_browsing l else e :: list_browsing l
    in
    list_browsing l
  in
  let rec check_list = function
    | [] -> []
    | e :: l -> e :: check_list (supp_divisor e l)
  in
  check_list (init_eratosthenes n)

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = ()

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c = try Some (input_line in_c) with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = ()

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l =
  match l with
  | [] ->
      failwith "Scalable.generate_primes.last_element: Youre list is empty. "
  | [ e ] -> e
  | h :: t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l =
  match l with
  | [] | [ _ ] ->
      failwith
        "Scalable.generate_primes.last_two: List has to have at least two \
         elements."
  | [ e; g ] -> (e, g)
  | h :: t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =
  let rec build_list = function
    | x when x = limit -> []
    | x ->
        if isprime x then
          let dp = add_b (mult_b x [ 0; 0; 1 ]) [ 0; 1 ] in
          if isprime dp then (x, dp) :: build_list (add_b x [ 0; 1 ])
          else build_list (add_b x [ 0; 1 ])
        else build_list (add_b x [ 0; 1 ])
  in
  build_list [ 0; 0; 1 ]

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec build_list = function
    | x when x = limit -> []
    | x ->
        if isprime x then
          let twin = add_b x [ 0; 0; 1 ] in
          if isprime twin then (x, twin) :: build_list (add_b x [ 0; 1 ])
          else build_list (add_b x [ 0; 1 ])
        else build_list (add_b x [ 0; 1 ])
  in
  build_list [ 0; 0; 1 ]
