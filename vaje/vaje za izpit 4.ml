(*1. naloga*)

(*a*)
let odstej_trojici (a, b, c) (x, y, z) = (a - x, b - y, c - z)

(*b*)
let rec max_rezultat_do_n f n = 
  if n <= 0 then f 0 else max (f n) (max_rezultat_do_n f (n - 1))

(*c*)
let rec pocisti_seznam list =
  let rec pocisti acc = function
    | None :: xs -> pocisti acc xs
    | Some x :: xs -> pocisti (x :: acc) xs
    | [] -> List.rev acc
  in
  pocisti [] list

(*d*)
let rec preveri_urejenost list =
  let rec narasca = function
    | [] | _ :: [] -> true
    | x1 :: x2 :: xs -> if x1 < x2 then narasca (x2 :: xs) else false
  in
  (list |> List.filter (fun x -> x mod 2 == 0) |> narasca)
  && (list |> List.filter (fun x -> x mod 2 == 1) |> List.rev |> narasca)

(*alt resitev, spisemo pomozni funkciji, ki preverita ali je seznam urejen, ter seznam razdelita na lihe in sode clene*)
let rec is_sorted_padajoce x = match x with
  | [] -> true
  | h :: [] -> true
  | h :: h2 :: t -> if h >= h2 then is_sorted_padajoce (h2 :: t) else false

let rec razdeli list =
  let rec aux list acc0 acc1 = 
    match list with
    | [] -> (List.rev acc0, List.rev acc1)
    | x :: xs -> if (x mod 2) = 0 then aux xs (x :: acc0) acc1 else aux xs acc0 (x :: acc1)
  in
  aux list [] []

let rec prva (a, b) = a
let rec druga (a, b) = b

let rec preveri_urejenost_moja list = is_sorted_padajoce (prva (razdeli list)) && is_sorted_padajoce (druga (razdeli list))


(*2. naloga*)
type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list

(*a*)
let gnezdenje_primer = [Element 1; Element 2; Podseznam [ Element 3; Podseznam [Element 4]; Podseznam []]; Podseznam [Element 5]]


(*b*)
let rec najvecja_globina = function
  | [] -> 1
  | Element _ :: xs -> najvecja_globina xs
  | Podseznam podsez :: xs -> max (1 + najvecja_globina podsez) (najvecja_globina xs)

(*c*)
let rec preslikaj f = function
  | [] -> []
  | Element x :: xs -> Element (f x) :: (preslikaj f xs)
  | Podseznam podsez :: xs -> Podseznam (preslikaj f podsez) :: (preslikaj f xs)

(*d*)
let rec splosci = function
  | [] -> []
  | Element x :: xs -> x :: splosci xs
  | Podseznam podsez :: xs -> splosci podsez @ splosci xs

(*e*)
let rec alternirajoci_konstruktorji = function
  | [] | _ :: [] -> true
  | Element _ :: (Podseznam _ :: xs as ys)
  | Podseznam _ :: (Element _ :: xs as ys) -> alternirajoci_konstruktorji ys
  | Element _ :: Element _ :: _
  | Podseznam _ :: Podseznam _ :: _ -> false