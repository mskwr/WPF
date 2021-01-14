(* Zadanie 4: Origami *)
(* Autor: Michał Skwarek *)
(* Code Review: Ewa Majdaniuk *)

(* Epsilon do ładnego porównywania liczb rzeczywistych *)
let eps = 0.000000001;;

(* Reprezentacja punktu na płaszczyźnie *)
type point = float * float;;

(* Typ informujący ile razy przebije kartkę szpilka wbita w danym punkcie *)
type kartka = point -> int;;

(* Funkcja zwracająca prostokątną kartkę *)
let prostokat (p1x, p1y) (p2x, p2y) (x, y) =
  if x >= p1x && x <= p2x && y >= p1y && y <= p2y then 1
  else 0;;

(* Funkcja licząca kwadrat danej liczby *)
let kwadrat x = x *. x;;

(* Funkcja zwracająca okrągłą kartkę *)
let kolko (px, py) r (x, y) =
  (* Równanie koła *)
  if kwadrat (x -. px) +. kwadrat (y -. py) -. kwadrat r <= eps then 1
  else 0;;

(* Funkcja informująca po której stronie prostej P1P2 leży punkt (x, y):       -1 jeśli na lewo od prostej, 0 jeśli na prostej i 1 jeśli na prawo *)
let polozenie (p1x, p1y) (p2x, p2y) (x, y) =
  (* Iloczyn wektorowy P1P i P1P2 *)
  let l = (x -. p1x) *. (p2y -. p1y) -. (p2x -. p1x) *. (y -. p1y) in
  if l -. 0. < eps && 0. -. l < eps then 0
  else if l > 0. then 1
  else -1;;

(* Funkcja zwracająca współczynniki prostej przechodzącej przez P1 i P2 *)
let prosta (p1x, p1y) (p2x, p2y) =
  let a = (p1y -. p2y) /. (p1x -. p2x) in
  let b = p1y -. p1x *. a in
  (a, b);;

(* Funkcja zwracająca współczynniki prostej przechodzącej przez punkt (x, y)
   i prostopadłej do prostej przechodzącej przez punkty P1 i P2 *)
let prostopadla (p1x, p1y) (p2x, p2y) (x, y) =
  let a = (p2x -. p1x) /. (p1y -. p2y) in
  let b = y -. a *. x in
  (a, b);;

(* Funkcja zwracająca punkt symetryczny do (x, y) względem prostej
   przechodzącej przez punkty P1 i P2 *)
let symetryczny (p1x, p1y) (p2x, p2y) (x, y) =
  (* Prosta równoległa do osi OY *)
  if p1x = p2x then (2. *. p1x -. x, y)
  (* Prosta równoległa do osi OX *)
  else if p1y = p2y then (x, 2. *. p1y -. y)
  else
    let (a, b) = prosta (p1x, p1y) (p2x, p2y)
    and (ap, bp) = prostopadla (p1x, p1y) (p2x, p2y) (x, y)
    in
    let przecieciex = (bp -. b) /. (a -. ap) in
    let przecieciey = a *. przecieciex +. b in
    (2. *. przecieciex -. x, 2. *. przecieciey -. y);;

(* Funkcja składająca kartkę wzdłuż danej prostej *)
let zloz p1 p2 f p =
  match polozenie p1 p2 p with
  (* Jeżeli punkt leży po lewej stronie prostej, szpilka trafia w dany punkt
     oraz w punkt symetryczny względem prostej przed zgięciem *)
  | -1 -> f p + f (symetryczny p1 p2 p)
  (* Jeżeli szpilka trafia w prostą, trafia w pojedynczy punkt zgięcia *)
  | 0 -> f p
  (* Jeżeli punkt leży po prawej stronie prostej, szpilka nie trafia *)
  | _ -> 0;;

(* Wielokrotne składanie kartki według prostych z danej listy *)
let skladaj l k = List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l;;
