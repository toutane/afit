(** A naive implementation of big integers

    This module aims at creating a set of big integers naively. Such data
    types will be subsequently called bitarrays. A bitarray is a list of
    zeros and ones ; first integer representing the sign bit. In this
    contexte zero is reprensented by the empty list []. The list is to
    be read from left to right ; this is the opposite convention to the
    one you usually write binary decompositions with. After the sign bit
    the first encountered bit is the coefficient in front of two to
    the power zero. This convention has been chosen to ease writing
    down code.

*)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  if x = 0 then []
  else
    let rec dec_to_bin n =
      if n <> 0 then (n mod 2) :: dec_to_bin (n / 2) else []
    in
    if x >= 0 then 0 :: dec_to_bin x else 1 :: dec_to_bin (-x)

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
*)
let to_int bA =
  if bA = [] then 0
  else
    let rec bin_to_dec = function
      | [] -> 0
      | b :: l -> (bin_to_dec l * 2) + b
    in
    match bA with
    | [] -> 0
    | e :: l -> if e = 0 then bin_to_dec l else -bin_to_dec l

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
*)
let print_b bA =
  let rec print = function
    | [] -> ()
    | b :: l ->
        print_int b;
        print l
  in
  print bA

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(*#install_printer print_b*)

(** Intelet rec inverse newList = function
    | [] -> newList
    | e :: l -> inverse l (e :: newList)
    inrnal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
*)

let rec compare_n nA nB =
  let rec inverse newList = function
    | [] -> newList
    | e :: l -> inverse (e :: newList) l
  in
  let trim l =
    let rec removeZero = function
      | [] -> []
      | e :: l -> if e = 1 then e :: l else removeZero l
    in
    removeZero (inverse [] l)
  in
  let rec length = function [] -> 0 | _ :: l -> 1 + length l in

  let tnA = trim nA and tnB = trim nB in
  let lenA = length tnA and lenB = length tnB in

  let rec compare = function
    | [], [] -> 0
    | [], _ | _, [] -> 0
    | e1 :: l1, e2 :: l2 ->
        if e1 = e2 then compare (l1, l2) else if e1 > e2 then 1 else -1
  in
  if lenA = lenB then compare (tnA, tnB) else if lenA > lenB then 1 else -1

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let ( >>! ) nA nB = match compare_n nA nB with 1 -> true | _ -> false

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let ( <<! ) nA nB = match compare_n nA nB with -1 -> true | _ -> false

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let ( >=! ) nA nB = match compare_n nA nB with -1 -> false | _ -> true

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let ( <=! ) nA nB = match compare_n nA nB with 1 -> false | _ -> true

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)

let compare_b bA bB =
  let rec areSignsSame = function
    | [], [] -> 0
    | [], l -> areSignsSame ([ 0; 0 ], l)
    | l, [] -> areSignsSame (l, [ 0; 0 ])
    | e1 :: l1, e2 :: l2 ->
        if e1 = e2 then compare_n l1 l2 else if e1 > e2 then -1 else 1
  in
  areSignsSame (bA, bB)

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let ( >> ) bA bB = match compare_b bA bB with 1 -> true | _ -> false

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let ( << ) bA bB = match compare_b bA bB with -1 -> true | _ -> false

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let ( >>= ) bA bB = match compare_b bA bB with -1 -> false | _ -> true

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let ( <<= ) bA bB = match compare_b bA bB with 1 -> false | _ -> true

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with [] -> 1 | e :: _ -> if e = 0 then 1 else -1

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
  match bA with [] -> [] | e :: l -> if e = 0 then e :: l else 0 :: l

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  let rec addition carry = function
    | [], [] -> if carry = 0 then [] else addition carry ([ 0 ], [ 0 ])
    | [], l -> addition carry ([ 0 ], l)
    | l, [] -> addition carry (l, [ 0 ])
    | e1 :: l1, e2 :: l2 -> (
        match (e1, e2) with
        | 0, 0 ->
            if carry = 0 then 0 :: addition carry (l1, l2)
            else 1 :: addition (carry - 1) (l1, l2)
        | 0, 1 | 1, 0 ->
            if carry = 0 then 1 :: addition carry (l1, l2)
            else 0 :: addition carry (l1, l2)
        | _ ->
            if carry = 0 then 0 :: addition (carry + 1) (l1, l2)
            else 1 :: addition carry (l1, l2))
  in
  addition 0 (nA, nB)

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let rec inverse newList = function
    | [] -> newList
    | e :: l -> inverse (e :: newList) l
  in
  let rec trim = function
    | [] -> []
    | e :: l -> if e = 0 then trim l else e :: l
  in
  let rec substract carry = function
    | [], [] -> if carry = 0 then [] else substract carry ([ 0 ], [ 0 ])
    | [], l -> substract carry ([ 0 ], l)
    | l, [] -> substract carry (l, [ 0 ])
    | e1 :: l1, e2 :: l2 -> (
        match (e1, e2) with
        | 0, 0 ->
            if carry = 0 then 0 :: substract carry (l1, l2)
            else 1 :: substract carry (l1, l2)
        | 1, 0 ->
            if carry = 0 then 1 :: substract carry (l1, l2)
            else 0 :: substract (carry - 1) (l1, l2)
        | 0, 1 ->
            if carry = 0 then 1 :: substract (carry + 1) (l1, l2)
            else 0 :: substract carry (l1, l2)
        | _ ->
            if carry = 0 then 0 :: substract carry (l1, l2)
            else 1 :: substract carry (l1, l2))
  in
  inverse [] (trim (inverse [] (substract 0 (nA, nB))))

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let add_b bA bB =
  match (bA, bB) with
  | [], [] -> []
  | e1 :: l1, e2 :: l2 -> (
      match (e1, e2) with
      | 0, 0 -> 0 :: add_n l1 l2
      | 0, 1 ->
          if compare_n l1 l2 > 0 then 0 :: diff_n l1 l2 else 1 :: diff_n l2 l1
      | 1, 0 ->
          if compare_n l2 l1 > 0 then 0 :: diff_n l2 l1 else 1 :: diff_n l1 l2
      | _, _ -> 1 :: add_n l1 l2)
  | [], l | l, [] -> l

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let rec diff_b bA bB =
  if bA = bB then []
  else
    match (bA, bB) with
    | [], [] -> []
    | [], e :: l -> if e = 0 then 1 :: l else 0 :: l
    | l, [] -> l
    | e1 :: l1, e2 :: l2 -> (
        match (e1, e2) with
        | 0, 0 ->
            if compare_n l1 l2 > 0 then 0 :: diff_n l1 l2 else 1 :: diff_n l2 l1
        | 0, 1 -> add_b bA (0 :: l2)
        | 1, 0 -> 1 :: add_n l1 l2
        | _, _ ->
            if compare_n l1 l2 > 0 then 1 :: diff_n l1 l2 else 0 :: diff_n l2 l1
        )

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let shift bA d =
  match bA with
  | [] -> []
  | e :: l ->
      let rec addZero n = if n > 0 then 0 :: addZero (n - 1) else l in
      e :: addZero d

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
  let rec multiply a b res =
    if b <> [] then
      match b with
      | [] -> []
      | 0 :: l -> multiply (0 :: a) l res
      | _ :: l ->
          let res2 = add_n res a in
          multiply (0 :: a) l res2
    else res
  in
  match (bA, bB) with
  | [], [] | [], _ | _, [] -> []
  | [ 0; 1 ], b | b, [ 0; 1 ] -> b
  | e1 :: l1, e2 :: l2 -> (
      match (e1, e2) with
      | 0, 0 -> 0 :: multiply l1 l2 []
      | 0, 1 -> 1 :: multiply l1 l2 []
      | 1, 0 -> 1 :: multiply l1 l2 []
      | _ -> 0 :: multiply l1 l2 [])

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =
  let rec len = function [] -> 0 | _ :: l -> 1 + len l in

  let rec inverse newList = function
    | [] -> newList
    | e :: l -> inverse (e :: newList) l
  in

  let trim l =
    let rec removeZero = function
      | [] -> []
      | e :: l -> if e = 1 then e :: l else removeZero l
    in
    removeZero (inverse [] l)
  in

  let unsign = function [] -> [] | _ :: l -> l in

  let rec lshift n l = if n > 0 then 0 :: lshift (n - 1) l else l in

  let rec rshift n = function
    | [] -> []
    | e :: l -> if n > 0 then rshift (n - 1) l else e :: l
  in

  let ubA = unsign bA
  and ubB = unsign bB
  and ucB = lshift (len bA - len bB) (unsign bB) in

  let rec loop a b q =
    if compare_n a ubB >= 0 then
      if compare_n a b >= 0 then
        let a2 = diff_n a b
        and b2 = rshift 1 b
        and q2 = add_n (lshift 1 q) [ 1 ] in
        if compare_n a2 ubB < 0 then
          let q3 = lshift (len b - len ubB) q2 in
          loop a2 b2 q3
        else loop a2 b2 q2
      else
        let b2 = rshift 1 b and q2 = lshift 1 q in
        loop a b2 q2
    else q
  in

  let uq = if bB == [] || compare_n ubA ubB < 0 then [] else loop ubA ucB [] in

  let signQ =
    match (bA, bB) with
    | e1 :: l1, e2 :: l2 -> (
        match compare_n l1 l2 with
        | -1 -> []
        | _ -> (
            let q = inverse [] (trim uq) in
            match (e1, e2) with
            | 0, 0 -> 0 :: q
            | 0, 1 -> 1 :: q
            | 1, 0 -> 1 :: q
            | _ -> 0 :: q))
    | _ -> []
  in

  let r = diff_b bA (mult_b bB signQ) in
  if sign_b r = -1 then diff_b signQ [ 0; 1 ] else signQ

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
*)
let mod_b bA bB =
  let q = quot_b bA bB in
  let r = diff_b bA (mult_b bB q) in
  let r2 =
    if sign_b r >= 0 then r
    else if sign_b bB >= 0 then add_b bB r
    else diff_b r bB
  in
  if sign_b r2 >= 0 then r2
  else if sign_b bB >= 0 then add_b bB r2
  else diff_b r2 bB

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = (quot_b bA bB, mod_b bA bB)
