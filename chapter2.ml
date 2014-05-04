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
module type ORDERED = sig
    type t
    val lt : t -> t -> bool
    val print : t -> unit
end;;

module UnbalancedSet =
    functor (Elt : ORDERED) ->
        struct
            type elem = Elt.t
            type tree = Empty | T of tree * elem * tree
            type set = tree
            let empty = Empty
            let rec member =
                fun x t ->
                    match t with
                    | Empty -> false
                    | T(a,y,b) ->
                            if (Elt.lt x y) then (member x a)
                            else if (Elt.lt y x) then (member x b)
                            else true
            let rec ins =
                fun x t ->
                    match t with
                    | Empty -> T(Empty, x, Empty)
                    | T(a,y,b) ->
                            if (Elt.lt x y) then T((ins x a),y,b)
                            else if (Elt.lt y x) then T(a,y,(ins x b))
                            else T(a,x,b)
        end;;

module OrderedInt : ORDERED =
    struct
        type t = int
        let lt = fun (x : int) (y : int) ->
            x < y
        let print = fun (x : int) ->
            print_int x
    end;;
module USetInt = UnbalancedSet(OrderedInt);;
USetInt.ins (OrderedInt.id 5) USetInt.empty;;

(*
            (USetInt.ins (OrderedInt.t 4)
              (USetInt.ins 6
                (USetInt.ins 8 USetInt.Empty) ) ) );;
print_bool member 5; print_string " "; print_bool member 10; print_string "\n";; *)
