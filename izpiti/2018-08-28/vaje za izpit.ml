(* 1. naloga *)

(* a *)
let razlika_kvadratov x y = (x + y)*(x + y) - ( x * x + y * y)

(* b *)
let uporabi_na_paru f (x, y) = (f x, f y)

(* c *)
let rec ponovi_seznam list n = 
  if n <= 0 then [] else list @ (ponovi_seznam list (n - 1))
  
(* d *)
let rec razdeli list =
  let rec aux acc_neg acc_poz list =
    match list with
    | [] -> (acc_neg, acc_poz)
    | x :: xs -> if x < 0 then aux (x :: acc_neg) acc_poz xs
    else aux acc_neg (x :: acc_poz) xs
  in 
  aux [] [] list


(*2. naloga *)

type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Empty

let leaf x = Node (Empty, x, Empty)

let monotona_pot tree = failwith "tree is dead"

(*3. naloga*)

(*a*)
type 'a veriga = 
  | Filter of ('a -> bool) * 'a list * 'a veriga 
  | Ostalo of 'a list

let test = Filter ( (>) 0, [], Filter ( (<) 10, [], Ostalo []))

(*b*)
let rec vstavi a = function
  | Ostalo xs -> Ostalo (a :: xs)
  | Filter (f, xs, veriga) -> if (f a) then Filter (f, a :: xs, veriga)
  else Filter (f, xs, vstavi a veriga)

(*c*)
let rec poisci a = function
  | Ostalo xs -> List.mem a xs 
  | Filter (f, xs, veriga) -> if (f a) then (List.mem a xs) 
  else poisci a veriga

(*d*)
let rec izprazni_filtre = function
  | Ostalo xs -> (Ostalo [], xs)
  | Filter (f, xs, veriga) ->  
  let empty, vsebina = izprazni_filtre veriga in
  (Filter (f, [], empty), xs @ vsebina)

(*e*)
let rec dodaj_filter f veriga = 
  let empty, vsebina = izprazni_filtre veriga in
  let nova_veriga = Filter (f, [], empty) in 
  List.fold_right vstavi vsebina nova_veriga
