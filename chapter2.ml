(* exercise 2.1 *)
let rec suffixes t = match t with | [] -> [[]] | (h::e) -> t :: suffixes e in suffixes [1,2,3,4];;
