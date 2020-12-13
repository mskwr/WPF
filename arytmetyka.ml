(* Zadanie 1: Arytmetyka *)
(* Autor: Michał Skwarek *)
(* Code Review: Dawid Mędrek *)

(* Jako Przedział traktuję przedział liczbowy w formie <a, b>.
 * Jako Dopelnienie traktuję przedział w formie <neg_inf, a> u <b, inf> *)
type wartosc =
  | Przedzial of float * float
  | Dopelnienie of float * float;;

(* Funkcja sprawdzająca, czy dana wartość nie jest przedziałem pustym *)
let pusty x =
  match x with
  | Dopelnienie (l, p) ->
    if l = neg_infinity && p = infinity then true
    else false
  | _ -> false;;

(* ------ KONSTRUKTORY ------ *)

let wartosc_dokladnosc x p =
  let blad = abs_float (x *. (p /. 100.))
  in
  Przedzial (x -. blad, x +. blad);;

let wartosc_od_do x y =
  Przedzial (x, y);;

let wartosc_dokladna x =
  Przedzial (x, x);;

(* ------ SELEKTORY ------ *)

let in_wartosc x w =
  match x with
  | Przedzial (l, p) -> w >= l && w <= p
  | Dopelnienie (l, p) -> w <= l || w >= p;;

let min_wartosc w =
  match w with
  | Przedzial (l, _) -> l
  | Dopelnienie (l, p) ->
    if pusty w then nan
    else neg_infinity;;

let max_wartosc w =
  match w with
  | Przedzial (_, p) -> p
  | Dopelnienie (l, p) ->
    if pusty w then nan
    else infinity;;

let sr_wartosc w =
  match w with
  | Przedzial (l, p) ->
    if l = -0. && p = -0. then 0.
    else if l != neg_infinity && p != infinity then (l +. p) /. 2.
    else nan
  | _ -> nan;;

(* ------ FUNKCJE POMOCNICZE ------ *)

(* Funkcja, która przekształca nam przedział na prawidłowy, jeśli nie 
   znamy wartości brzegowe, ale nie wiemy w jakiej są kolejności. Warunek
   var pozwala na odpowiednią regulację na Przedział i Dopełnienie. *)
let mZakres a b var =
  if var = 0 then Przedzial (min a b, max a b)
  else Dopelnienie (min a b, max a b);;

(* Funkcja zwracająca najmniejszy spośród czterech par iloczynów. Funkcja
   pomocnicza usun_nan eliminuje z tych iloczynów niechcianą wartość nan,
   która może powstać na przykład podczas mnożenia 0 * infinity. *)
let kdolny l1 p1 l2 p2 =
  let usun_nan a b =
    if classify_float (a *. b) = FP_nan then infinity
    else a *. b
  in
  let a = usun_nan l1 l2
  and b = usun_nan l1 p2
  and c = usun_nan p1 l2
  and d = usun_nan p1 p2
  in
  min (min a b) (min c d);;

(* Funkcja analogiczna do kdolny, tylko zwracająca największy iloczyn. *)
let kgorny l1 p1 l2 p2 =
  let usun_nan a b =
    if classify_float (a *. b) = FP_nan then neg_infinity
    else a *. b
  in
  let a = usun_nan l1 l2
  and b = usun_nan l1 p2
  and c = usun_nan p1 l2
  and d = usun_nan p1 p2
  in
  max (max a b) (max c d);;

(* Funkcja sprawdzająca dany przedział zawiera 0. *)
let zer x =
  if in_wartosc x 0. then true
  else false;;

(* Funkcja sprawdzająca czy dane dopełnienie nie jest dopełnieniem
   przedziału pustego - wówczas przekształca je na Przedział <-inf, inf> *)
let check x =
  match x with
  | Dopelnienie (l, p) ->
    if l >= p then Przedzial (neg_infinity, infinity)
    else x
  | _ -> x;;

(* Funkcja zwracająca przedział/dopełnienie przeciwne do zadanego. *)
let przeciwny x =
  match x with
  | Przedzial (l, p) -> Przedzial (-.p, -.l)
  | Dopelnienie (l, p) -> Dopelnienie (-.p, -.l);;

(* Funkcja zwracająca przedział/dopełnienie odwrotne do zadanego *)
let odwrotny x =
  match x with
  | Przedzial (l, p) ->
    (* Odwrotność zera jako przedział pusty. *)
    if l = 0. && p = 0. then Dopelnienie (neg_infinity, infinity)
    else if l = 0. then Przedzial (1. /. p, infinity)
    else if p = 0. then Przedzial (neg_infinity, 1. /. l)
    (* Jeśli Przedział przyjmuje wartości w otoczeniu zera, to wówczas
       przechodzi na dopełnienie (rozszerza się do infinity). *)
    else if zer x then mZakres (1. /. p) (1. /. l) (1)
    else mZakres (1. /. p) (1. /. l) (0)
  | Dopelnienie (l, p) ->
    (* Odwrotność przedziału pustego jako przedział pusty. *)
    if pusty x then Dopelnienie (neg_infinity, infinity)
    else if zer x then mZakres (1. /. p) (1. /. l) (1)
    (* Dopelnienie przedziału niezawierającego zera przejdzie
       na Przedzial, które je zawiera. *)
    else mZakres (1. /. p) (1. /. l) (0);;

(* ------ MODYFIKATORY ------ *)

let rec plus a b =
  match a, b with
  | Dopelnienie (l1, p1), Dopelnienie (l2, p2) ->
    (* Jeśli któryś z przedziałów jest pusty, suma też jest pusta. *)
    if pusty a || pusty b then Dopelnienie (neg_infinity, infinity)
    else Przedzial (neg_infinity, infinity)
  | Przedzial (l1, p1), Dopelnienie (l2, p2) ->
    if pusty b then Dopelnienie (neg_infinity, infinity)
    (* Sprawdzenie czy nie powstało dopełnienie przedziału pustego. *)
    else check (Dopelnienie (l2 +. p1, p2 +. l1))
  | Dopelnienie (l1, p1), Przedzial (l2, p2) ->
    (* Przypadek analogiczny jak powyższy, tylko z inną kolejnością. *)
    plus b a
  | Przedzial (l1, p1), Przedzial (l2, p2) ->
    Przedzial (l1 +. l2, p1 +. p2);;

let minus a b =
  plus (a) (przeciwny b);;

let rec razy a b =
  match a, b with
  | Dopelnienie (l1, p1), Dopelnienie (l2, p2) ->
    (* Jeśli któryś z przedziałów jest pusty, to iloczyn również. *)
    if pusty a || pusty b then Dopelnienie (neg_infinity, infinity)
    (* Zawieranie otoczenia zera rozszerza przedział na pełny. *)
    else if zer a || zer b then Przedzial (neg_infinity, infinity)
    (* Ograniczenia górne i dolne na iloczyn. *)
    else
      let l = max (l1 *. p2) (l2 *. p1)
      and p = min (p1 *. p2) (l1 *. l2)
      in check (Dopelnienie (l, p))
  | Dopelnienie (l1, p1), Przedzial (l2, p2) ->
    if pusty a then Dopelnienie (neg_infinity, infinity)
    else if l2 = 0. && p2 = 0. then Przedzial (0., 0.)
    else if zer b then Przedzial (neg_infinity, infinity)
    (* Przypadek analogiczny do tego poniżej, tylko ze zmianą znaku. *)
    else if l2 <= 0. then przeciwny (razy (a) (przeciwny b))
    else
      let l = max (l1 *. p2) (l1 *. l2)
      and p = min (p1 *. p2) (p1 *. l2)
      in check (Dopelnienie (l, p))
  | Przedzial (_, _), Dopelnienie (_, _) ->
    (* Przypadek analogiczny jak powyższy, tylko z inną kolejnością. *)
    razy b a
  | Przedzial (l1, p1), Przedzial (l2, p2) ->
    if (l1 = 0. && p1 = 0.) || (l2 = 0. && p2 = 0.) then Przedzial (0., 0.)
    (* Możliwe ograniczenia na zadany przedział. *)
    else Przedzial (kdolny l1 p1 l2 p2, kgorny l1 p1 l2 p2);;

let podzielic a b =
  razy (a) (odwrotny b);;
