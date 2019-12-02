(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = if x * x = y then true else false 

let pack3 x y z = (x, y, z)

let sum_if_not f list = 
  let rec sum_aux f list acc =
    match list with
      | [] -> acc
      | x :: xs -> if not f x then acc + x else sum_aux f xs acc
      in
      sum_aux list 0  

(* acc naj bo 0, ce pogoj ni izpoljen, mu pristejemo x in dobimo vsoto elementov, za ketere pogoj ni izpoljnen, ce pa je seznam prazen, pristejemo 0 oz ne spremenimo acc *)

let rec apply list_f list_n =
  match (list_f, list_n) with
  | (_, []) -> []
  | (f :: fs, x :: xs) -> [f x :: (apply fs x)] @ [apply fs xs]


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = 
  | Predavanja
  | Vaje

type srecanje = {predmet: string; vrsta_srecanja: string; trajanje: float}

type urnik = Urnik of list list

let vaje = {predmet = "Analiza 2a"; vrsta_srecanja = Vaje; trajanje = 3.  }
let predavanje = {predmet = "Programiranje 1"; vrsta_srecanja = Predavanja; trajanje = 2.}

let urnik_profesor = Urnik([[{predmet = "Algebra"; vrsta_srecanja = Vaje; trajanje = 2.}],
[]; [{predmet = "Algebra"; vrsta_srecanja = Predavanja; trajanje = 1.}],
[]; []; [{predmet = "Algebra"; vrsta_srecanja = Vaje; trajanje = 2.}]; []]) (* imamo seznam seznamov, vsak podseznam predtsavlja en delovani dan*)

let rec je_preobremenjen urnik = 
  let year =
  match vrsta_srecanja with
  | Predavanja -> 4
  | Vaje -> 4
  in 
  let rec aux acc urnik = function
  | [] -> acc
  | [[{predmet; vrsta_srecanja; trajanje}] :: rest] ->
  match (vrsta_srecanja, trajanje) with
  | (Predavanja, n) -> if (acc + n) >= 4 then true else aux acc rest
  | (Vaje, n) -> if (acc + n) >= 4 then true else aux acc rest
  in
  aux 0 urnik 



let bogastvo () = failwith "dopolni me"
