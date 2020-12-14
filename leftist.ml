(* Zadanie 2: Drzewa lewicowe *)
(* Autor: Michał Skwarek *)
(* Code Review: Olaf Placha *)

(* Typ złączalnej kolejki priorytetowej: *)
(* Null: kolejka jest pusta *)
(* Node: priorytet * prawa wysokość * lewe poddrzewo * prawe poddrzewo *)
type 'a queue =
  | Null
  | Node of 'a * int * 'a queue * 'a queue;;

(* Konstruktor pustej kolejki *)
let empty = Null;;

(* Wyjątek podnoszony przez delete_min, gdy kolejka jest pusta *)
exception Empty;;

(* Funkcja, która zwraca true, gdy kolejka jest pusta i false wpp *)
let is_empty q =
  match q with
  | Null -> true
  | _ -> false;;

(* Funkcja złączająca ze sobą dwie kolejki *)
let rec join q1 q2 =
  match q1, q2 with
  (* Łączenie danej kolejki z kolejką pustą jej nie zmienia *)
  | Null, q2 -> q2
  | q1, Null -> q1
  | Node (p1, d1, left1, right1), Node (p2, d2, left2, right2) ->
    (* Zamiana kolejności argumentów, aby korzeń pierwszego zawierał
       element o mniejszym priorytecie niż korzeń drugiego *)
    if p1 > p2 then join q2 q1
    else
      (* Złączenie prawego poddrzewa q1 i całego drzewa q2 *)
      let q3 = join right1 q2 in
      match left1, q3 with
      (* Gdy korzeń ma tylko jedno poddrzewo, prawa wysokość to 1 *)
      | Null, q3 -> Node (p1, 1, q3, Null)
      | left1, Null -> Node (p1, 1, left1, Null)
      | Node (_, d_left1, _, _), Node (_, d_q3, _, _) ->
        (* Złączenie lewego poddrzewa q1 i całego drzewa q3 *)
        if d_left1 < d_q3 then Node (p1, d_left1 + 1, q3, left1)
        else Node (p1, d_q3 + 1, left1, q3);;

(* Funkcja złączająca dane drzewo/kolejkę z pojedynczym liściem *)
let add e q =
  join (Node (e, 1, Null, Null)) (q);;

(* Funkcja zwracająca parę złożoną z priorytetu elementu zawartego w
   korzeniu danego drzewa i kolejki bez tego elementu minimalnego *)
let delete_min q =
  match q with
  | Null -> raise Empty
  | Node (p, d, left, right) -> (p, join left right);;
