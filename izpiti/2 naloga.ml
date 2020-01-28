(*2.naloga*)

type 'a improved_list =
  | Empty
  | Node of 'a array * 'a improved_list

let test_list = Node([|1; 2; 20|], Node([|17; 19; 20 ;30|], Node([|100|], Empty)))

let rec count = function
  | Empty -> 0
  | Node(a, list) -> Array.length a + count list

let rec nth i = function
  | Empty -> None
  | Node(a, list) -> if i < Array.length a then Some (Array.get a i) else nth (i - Array.length a) list

(*definiramo pomozno funkcijo, ki preveri, ali je seznam urejen; naprej damo elemente improved_list v sezna, in za tega preverimo, da je urejen*)
let rec list_is_sorted = function
  | [] -> true
  | x :: [] -> true
  | x :: y :: xs -> if x <= y then list_is_sorted (y :: xs) else false 

let rec is_sorted = function
  | Empty -> true
  | Node(a, list) -> if (list_is_sorted (Array.to_list a)) = true then is_sorted list else false

let rec update i vrednost replacement = function
  | Empty -> Empty
  | Node(a, list) -> if i < Array.length a then Node(Array.set a i replacement, list) else update (i - Array.length a) vrednost replacement list

  (*Array.copy da shranimo original tabelo?*)

let test_array = [|1; 2; 20|]

