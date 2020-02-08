(*1. naloga*)

let rec print_all list =
  match list with
  | [] -> ()
  | x :: xs -> print_int x; print_all xs

let rec map2_opt f list1 list2 =
  if List.length list1 <> List.length list2 then None
  else let rec aux f list1 list2 acc = 
    match list1, list2 with
    | [], [] -> Some (List.rev acc)
    | _, [] -> None
    | [], _ -> None
    | x :: xs, y :: ys -> aux f xs ys (f x y :: acc)
  in aux f list1 list2 []

(*2. naloga*)

type filter_tree = 
  | Node of int * filter_tree * filter_tree
  | Box of int list

let test_tree = Node(10, Node(5, Box([1]), Box([])), Node(15, Box([]), Box([19; 20])))

let rec vstavi x = function
  | Box([]) -> Box([x])
  | Box(y :: ys) -> Box(x :: y :: ys)
  | Node(k, lt, rt) when x <= k -> Node(k, vstavi x lt, rt)
  | Node(k, lt, rt) -> Node(k, lt, vstavi x rt)

let rec vstavi_seznam list f_tree = 
  match list with
  | [] -> f_tree
  | x :: xs -> vstavi_seznam xs (vstavi x f_tree)

let rec insert_many l ftree =
  List.fold_right vstavi l ftree

let rec boxed_correctly ftree =
  let checker lower upper x =
    match (lower, upper) with
    | (None, None) -> true
    | (Some l, None) -> l <= x
    | (None, Some u) -> x < u
    | (Some l, Some u) -> l <= x && x < u
  in
  let rec values_between lower upper ftree =
    match ftree with
    | Box(xs) -> List.for_all (checker lower upper) xs
    | Node(f, lt, rt) ->
      (values_between lower (Some f) lt) && (values_between (Some f) upper rt)
  in
  values_between None None ftree


(*3.naloga*)

module type Linear = sig
  type t
  type vektor = int * int
  type matrika = int * int * int * int

  val id : t
  val uporabi : t -> vektor -> vektor
  val iz_matrike : matrika -> t
  val iz_funkcije : (vektor -> vektor) -> t 
  val kompozitum : t -> t -> t
end

module Matrika : Linear = struct
  type t = int * int * int * int

  let id = (1, 0, 0, 1)
  let uporabi (a, b, c, d) (x, y) = (a * x + b * y, c * x + b * y)
  let iz_matrike m = m
  let iz_funkcije f =
    let (a, c) = f (1, 0) in
    let (b, d) = f (0, 1) in
    (a, b, c, d)
  let kompotizum (a, b, c, d) (e, f, g, h) = (a * e + b * g, a * f + b * h, c * e + d * g, c * f + d * h)
end  

module Funkcija : Linear = struct
  type vektor = int * int -> int * int

  let id = (fun x -> x)
  let uporavi f v = f v
  let iz_matrike (a, b, c, d) = fun (x, y) -> (a * x + b * y, c * x + b * y)
  let iz_funkcije f = f
  let kompozitum f g = fun x -> f (g x)
end


(*4.naloga*)

let test_matrix = 
[| [| 2 ; 4 ; 1 ; 1 |];
   [| 3 ; 2 ; 0 ; 5 |];
   [| 8 ; 0 ; 7 ; 2 |] |]
