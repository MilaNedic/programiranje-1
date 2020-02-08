(*1.naloga*)

let uporabi f x = f x

let ibaropu x f = f x

let zacetnih n xs =
  let rec aux n acc xs =
    if n <= 0 then
      Some (List.rev acc)
    else
      match xs with
      | [] -> None
      | x::xs -> aux (n - 1) (x :: acc) xs
  in
  aux n [] xs

(*2.naloga*)

type 'a neprazen_sez = 
  | Konec of 'a 
  | Sestavljen of 'a * 'a neprazen_sez

let rec prvi = function
  | Konec x -> x
  | Sestavljen(x, _) -> x

let rec zadnji = function
  | Konec x -> x
  | Sestavljen(_, rest) -> zadnji rest

let rec dolzina = function
  | Konec x -> 1
  | Sestavljen (x, rest) -> 1 + dolzina rest

let rec pretvori_v_seznam n_sez = 
  let rec aux n_sez acc =
    match n_sez with
    | Konec x -> List.rev (x :: acc)
    | Sestavljen(x, rest) -> aux rest (x :: acc)
  in
  aux n_sez []

let rec zlozi f s = function
  | Konec x -> f s x
  | Sestavljen(x, rest) -> zlozi f (f s x) rest

