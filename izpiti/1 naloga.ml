(*1.naloga*)

(*a*)
let option_sum (x:int) (y:int) : int = x + y

(*b*)
let twostep_map f f1 f2 x = (f1 (f x), f2 (f x)) (*najprej preslikamo x v par (f x, f x), nato na teh dveh elementih uporabimo f1 in f2*)

(*c*)
(*definiramo pomozno funkcijo, ki x kopira n-krat*)
let rec repeat x n = if n <= 0 then [] else x :: (repeat x (n-1))

let rec function_repeat f list = 
  let rec aux f list acc = function
    | [] -> List.rev acc (*ko pridemo do praznega seznama, g aobrnemo, da dobimo prvotni vrstni red elementov, le da so ti elementi ponovljeni*)
    | x :: xs -> aux f xs ((repeat x (f x)) @ acc) (*x ponovimo f x-krat, če le ta ni pozitiven, to je nas v v repeat x n; seznam, ki ga dobimo, dodamo v akumulator acc *)
    (*funkcije repeat ze preveri, ce je nas n, torej f x <= 0, torej je ''if f x <= 0 then aux f xs acc'' nepotreben *)
  in 
  aux f list []

(*d*)
let rec iterate f stop start_value = 
  if f start_value <> stop then iterate f stop (f start_value) else f start_value


