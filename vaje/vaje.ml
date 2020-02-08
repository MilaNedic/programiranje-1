(*1. naloga*)

let rec razlika_kvadratov x y = (x + y)*(x + y) - (x*x + y*y)

let rec uporabi_na_paru f (x, y) = (f x, f y)

let rec ponovi_seznam list n = if n <= 0 then [] else list @ (ponovi_seznam list (n - 1))

let rec razdeli list =
  let rec aux list acc_neg acc_rest =
    match list with
      | [] -> (List.rev acc_neg, List.rev acc_rest)
      | x :: xs -> if x < 0 then aux xs (x :: acc_neg) acc_rest else aux xs acc_neg (x :: acc_rest)
  in 
  aux list [] []


(*3.naloga*)
type 'a veriga = 
  | Filter of ('a -> bool) * 'a list * 'a veriga
  | Ostalo of 'a list

let test = Filter((fun x -> x < 0), [], Filter((fun x -> x < 10), [], Ostalo([])))

let rec vstavi x = function
  | Ostalo(sez) -> Ostalo(x :: sez)
  | Filter(f, sez, veriga) when f x -> Filter(f, x :: sez, veriga)
  | Filter(f, sez, veriga) -> Filter(f, sez, vstavi x veriga)

let rec poisci x = function
  | Ostalo(sez) -> List.mem x sez
  | Filter(f, sez, veriga) when f x -> List.mem x sez
  | Filter(f, sez, veriga) -> poisci x veriga

let rec izprazni_filtre = function
  | Ostalo(sez) -> (Ostalo([]), sez)
  | Filter (f, sez, veriga) ->
    let prazni, vsebina = izprazni_filtre veriga in
    (Filter (f, [], prazni), sez @ vsebina)

let rec dodaj_filter f veriga = 
  let prazni, vsebina = izprazni_filtre veriga in
  let nova_veriga = Filter(f, [], prazni) in
  List.fold_right vstavi vsebina nova_veriga

(*2.naloga*)

type 'a tree = 
  | Empty
  | Node of ('a tree) * 'a * ('a tree)

let leaf x = Node (Empty, x, Empty)

let test1 = Node(Node(leaf 3, 10, Node(leaf 14, 13, leaf 6)), 11, Node(leaf 2, 8, leaf 10))

let longer_of lst1 lst2 =
  if List.length lst1 > List.length lst2 then lst1 else lst2

let rec decreasing upper_bound = function
  | Empty -> []
  | Node (l, x, r) when x > upper_bound -> []
  | Node (l, x, r) -> x :: longer_of (decreasing x l) (decreasing x r)

let rec increasing lower_bound = function
  | Empty -> []
  | Node (l, x, r) when x < lower_bound -> []
  | Node (l, x, r) -> x :: longer_of (increasing x l) (increasing x r)

let rec longest = function
  | Empty -> []
  | Node (l, x, r) ->
      let combined1 = List.rev (decreasing x l) @ [x] @ increasing x r in
      let combined2 = List.rev (increasing x l) @ [x] @ decreasing x r in
      let long_combined = longer_of combined1 combined2 in
      let long_subtrees = longer_of (longest l) (longest r) in
      longer long_combined long_subtrees