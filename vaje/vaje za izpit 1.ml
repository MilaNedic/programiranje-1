(*Vaje za izpit: 24.1.2019*)

(*1. naloga*)

(*a*)
let podvoji_vsoto a b = 2 * (a + b)

(*b*)
let povsod_vecji (a, b, c) (x, y, z) = if a > x && b > y && c > z then true else false

(*c*)
let uporabi_ce_lahko f = function
  | None -> None
  | Some x -> Some (f x)

(*d*)
let rec pojavi_dvakrat x lst =
  let rec prestej = function
  | [] -> 0
  | y :: ys -> if x = y then 1 + prestej ys else prestej ys
  in
  prestej lst = 2

(*e*)
let izracunaj_v_tocki p list_of_functions =
  let rec aux acc = function
    | [] -> List.rev acc (*ohraniti zelimo vrsti red elementov v acc, da dobimo rpvotni vrstni red*)
    | f :: fs -> aux (f p :: acc) fs
  in 
  aux [] list_of_functions

(*f*)
let rec eksponent x p = if p <= 0 then 1 else x * eksponent x (p - 1)

let rec eksponent' x p =
  let rec aux x p acc =
    if p <= 0 then acc else aux x (p - 1) (x * acc)
  in
  aux x p 1


(*2. naloga*)

(*a*)
type 'a mm_drevo =
  | Empty
  | Node of 'a mm_drevo * 'a * int * 'a mm_drevo

(*b*)
let rec vstavi x = function
  | Empty -> Node(Empty, x, 1, Empty)
  | Node(lt, y, n, rt) when x = y -> Node(lt, y, n + 1, rt)
  | Node(lt, y, n, rt) when y < x -> Node(vstavi x lt, y, n, rt)
  | Node(lt, y, n, rt) -> Node(lt, y, n, vstavi x rt)

(*c*)
let rec multimnozica_iz_seznama = List.fold_left (fun mmtree x -> vstavi x mmtree) Empty

(*d*)
let rec velikost_multimnozice = function
  | Empty -> 0
  | Node(lt, _, n, rt) -> n + velikost_multimnozice lt + velikost_multimnozice rt

(*e*)
let rec ponovi x n = if n <= 0 then [] else x :: ponovi x (n - 1)

let rec seznam_iz_multimnozice = function
  | Empty -> []
  | Node(lt, x, n, rt) -> (seznam_iz_multimnozice lt) @ (ponovi x n) @ (seznam_iz_multimnozice rt)
