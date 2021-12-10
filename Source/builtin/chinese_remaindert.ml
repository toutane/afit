(** Chinese remainder theorem *)

open Builtin
open Basic_arithmetics

(** Image of the Chinese Remainder map
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
 *)
let crt_image x l =
  let rec findRemainders = function
      [] -> []
    | p::l -> (modulo x p)::(findRemainders l)
  in findRemainders l
;;

let rec print_list = function
    [] -> ()
  | r::l -> print_int r; print_newline(); print_list l
;;

(*print_list (crt_image 23 [3; 5; 7]);;*)

(** Inverse image of Chinese Remainder map
    @para m a positive integer
    @param l list of pairwise relatively prime factors of m
    @param y list of remainders modulo pairwise relatively prime factors of m
 *)

let crt_solver m l y =
  let rec initPartialProductList = function
      [] -> []
    | p::l -> (quot m p)::(initPartialProductList l)
  in let partialProductList = initPartialProductList l
     in let rec getIeme i = function
            [] -> invalid_arg "list too short"
          | e::l -> if i = 1 then e
                    else getIeme (i - 1) l
        in let rec initModularInverseList i = function
               [] -> []
                   | p::l -> let (d, _, _) = bezout (getIeme i partialProductList) p
                             in if d != 1 then (modulo (getIeme i partialProductList) p)::(initModularInverseList (i + 1) l)
                                  else d::(initModularInverseList (i + 1) l)
           in let inverseList = initModularInverseList 1 l
              in let rec sum i = function
                     [] -> 0
                   | rem::l -> (getIeme i partialProductList) * (getIeme i inverseList) * rem + (sum (i + 1) l)
                 in let sum = sum 1 y
                    in (modulo sum m)
;;

(*print_int (crt_solver 105 [3; 5; 7] [2; 3; 2]);;*)
