let rec print_int_list l =
    match l with
    | [] -> ()
    | e::t -> print_int e ; print_string " " ; print_int_list t;;

let rec print_int_list_list =
    function
        [] -> ()
    | e::t -> print_string "[ ";
              print_int_list e;
              print_string "] ";
              (print_int_list_list t);;

(* exercise 2.1 *)
print_string "2.1)\n"; print_string "    ";
let rec suffixes =
   function
      [] -> [[]]
      | h::t -> ((h::t) :: suffixes t) in
print_int_list_list (suffixes [1;2;3;4]);
print_string "\n";;

(* definitions needed from figure 2.9 *)
module type Ordered = sig
    val lt : 'a -> 'a -> bool
    val eq : 'a -> 'a -> bool
end;;
