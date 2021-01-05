(* Zadanie 3: Modyfikacja drzew *)
(* Autor: Michał Skwarek *)
(* Code Review: Jakub Korzeniewski *)

(* Typ zbioru przedziałowego: *)
(* Empty: drzewo jest puste *)
(* Node: lpoddrzewo * przedział * ppoddrzewo * wysokość * l. elementów *) 
type t =
  | Empty
  | Node of t * (int * int) * t * int * int;;

(* Funkcja, która zwraca wysokość danego drzewa *)
let height t =
  match t with
  | Node (_, _, _, h, _) -> h
  | Empty -> 0;;

(* Funkcja, która zwraca ilość liczb we wszystkich poddrzewach *)
let count t =
  match t with
  | Node (_, _, _, _, c) -> c
  | Empty -> 0;;

(* Bezpieczne dodawanie liczb względem wartość int_min i int_max *)
let safe_add a b =
  let a = min a b
  and b = max a b
  in
  if a >= 0 then
    if a >= (max_int - b) then max_int
    else a + b
  else if b <= (min_int - a) then min_int
  else a + b;;

(* Funkcja składająca dwa poddrzewa z danym przedziałem, rozłącznym
   względem nich *)
let make l (a, b) r =
  let k = (a, b)
  and (a, b) =
    (* zabezpieczenie, aby sum2 na pewno nie wyszła poza zakres *)
    if a = min_int then (a + 1, safe_add b 1)
    else (a, b)
  in
  let maxheight = (max (height l) (height r)) + 1
  (* suma elementów w obu poddrzewach *)
  and sum1 = safe_add (count l) (count r)
  (* ilość liczb całkowitych w podanym przedziale *)
  and sum2 = safe_add (safe_add b (-a)) 1
  in
  Node (l, k, r, maxheight, safe_add (sum1) (sum2));;

(* Funkcja, która balansuje drzewo z jego poddrzewami i danym przedziałem *)
let bal l k r =
  let hl = height l
  and hr = height r
  in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
      if height ll >= height lr then make ll lk (make lr k r)
      else
        (match lr with
        | Node (lrl, lrk, lrr, _, _) ->
          make (make ll lk lrl) lrk (make lrr k r)
        | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
      if height rr >= height rl then make (make l k rl) rk rr
      else
        (match rl with
         | Node (rll, rlk, rlr, _, _) ->
           make (make l k rll) rlk (make rlr rk rr)
         | Empty -> assert false)
    | Empty -> assert false
  else make l k r;;

(* Funkcja zwracająca najmniejszy element w drzewie *)
let rec min_elt t =
  match t with
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found;;

(* Funkcja usuwająca najmniejszy element w drzewie, jeśli taki istnieje *)
let rec remove_min t =
  match t with
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min l) k r
  | Empty -> t;;

(* Funkcja zwracająca największy element w drzewie *)
let rec max_elt t =
  match t with
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> raise Not_found;;

(* Funkcja usuwająca najwiekszy element w drzewie, jeśli taki istnieje *)
let rec remove_max t =
  match t with
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max r)
  | Empty -> t;;

(* Funkcja zwracająca 0, jeśli dwa dane przedziały mają niepuste przecięcie
   i w przeciwnym wypadku: -1, jeśli pierwszy przedział jest mniejszy od
   drugiego oraz 1, jeśli jest większy *)
let intersection (a, b) (c, d) =
  if b < c then -1
  else if a > d then 1
  else 0;;

(* Funkcja dodająca do drzewa przedział, który w nim nie występuje *)
let rec add_one x t =
  match t with
  | Node (l, k, r, _, _) ->
    if (intersection x k < 0) then bal (add_one x l) k r
    else bal l k (add_one x r)
  (* Stworzenie węzła z danym przedziałem i bez synów *)
  | Empty -> make Empty x Empty;;

(* Funkcja składająca dwa rozłączne drzewa z takim przedziałem, że ten
   jest dołączany na szczyt drzewa (l < k < r) *)
let rec join l k r =
  match (l, r) with
  | (Empty, _) -> add_one k r
  | (_, Empty) -> add_one k l
  | (Node (ll, lk, lr, lh, _), Node (rl, rk, rr, rh, _)) ->
    if rh + 2 < lh then bal ll lk (join lr k r)
    else if rh > lh + 2 then bal (join l k rl) rk rr
    else make l k r;;

(* Funkcja zwracająca pusty zbiór *)
let empty = Empty;;

(* Funkcja sprawdzająca, czy dany zbiór jest pusty *)
let is_empty t =
  match t with
  | Empty -> true
  | _ -> false;;

(* Funkcja zwracająca trójkę wartości w taki sposób, że: pierwsza z nich
   to drzewo zawierające elementy mniejsze od danego int x, druga to
   wartość logiczna odpowiadająca na to, czy drzewo zawiera element równy 
   x, a trzecia z nich to drzewo zawierające elementy większe od x *)
let split x t =
  let rec loop x t =
    match t with
    | Empty -> (Empty, false, Empty)
    | Node (l, (a, b), r, _, _) ->
      let i = intersection (x, x) (a, b) in
      if i = 0 then
        let left =
          if x > a then add_one (a, x - 1) l
          else l
        and right =
          if x < b then add_one (x + 1, b) r
          else r
        in
        (left, true, right)
      else if i < 0 then
        let (ll, kl, rl) = loop x l in
        (ll, kl, join rl (a, b) r)
      else
        let (lr, kr, rr) = loop x r in
        (join l (a, b) lr, kr, rr)
  in
  loop x t;;

(* Funkcja, która dodaje zadany przedział do danego zbioru *)
let add (a, b) t =
  (* Wyznaczam drzewo zawierające mniejsze elementy od a i drzewo
     zawierające większe elementy od b *)
  let (l, _, _) = split a t
  and (_, _, r) = split b t
  in
  (* Znajduję największy przedział w drzewie l i najmniejszy w r *)
  let (l1, l2) =
    if is_empty l then (safe_add a (-2), safe_add a (-2))
    else max_elt l
  and (r1, r2) =
    if is_empty r then (safe_add b (-2), safe_add b (-2))
    else min_elt r
  in
  (* Sprawdzam, czy przedziały będą sąsiednie, jeśli tak, to je sklejam *)
  if safe_add l2 1 = a then
    if safe_add b 1 = r1 then join (remove_max l) (l1, r2) (remove_min r)
    else join (remove_max l) (l1, b) r
  else if safe_add b 1 = r1 then join l (a, r2) (remove_min r)
  else join l (a, b) r;;

(* Funkcja, która usuwa zadany przedział z danego zbioru *)
let remove (a, b) t =
  (* Wyznaczam drzewo zawierające mniejsze elementy od a i drzewo
     zawierające większe elementy od b *)
  let (l, _, _) = split a t
  and (_, _, r) = split b t
  in
  match (l, r) with
  (* Dwa przypadki, gdy przedział okazał się skrajny w całym zbiorze *)
  | (Empty, _) -> r
  | (_, Empty) -> l
  (* Zastępuję ten przedział najmniejszym przedziałem z drzewa r *)
  | _ -> join l (min_elt r) (remove_min r);;

(* Funkcja, która sprawdza, czy zadane x występuje w danym zbiorze *)
let mem x t =
  let rec loop t =
    match t with
    | Node (l, k, r, _, _) ->
      let i = intersection (x, x) k in
      if i = 0 then true
      else if i < 0 then loop l
      else loop r
    | Empty -> false
  in
  loop t;;

(* Funkcja wywołująca zadaną funkcję z każdym przedziałem danego zbioru *)
let iter f t =
  let rec loop t =
    match t with
    | Node (l, k, r, _, _) -> loop l; f k; loop r
    | Empty -> ()
  in
  loop t;;

(* Funkcja zwracająca [(f xN ... (f x2 (f x1 a))...)], gdzie x_i to
   przedziały uporządkowane rosnąco *)
let fold f t acc =
  let rec loop acc t =
    match t with
    | Node (l, k, r, _, _) -> loop (f k (loop acc l)) r
    | Empty -> acc
  in
  loop acc t;;

(* Funkcja zwracająca listę przedziałów zbioru uporządkowanych rosnąco *)
let elements t =
  let rec loop acc t =
    match t with
    | Node (l, k, r, _, _) -> loop (k :: loop acc r) l
    | Empty -> acc
  in
  loop [] t;;

(* Funkcja, która zwraca ilość liczb w całym zadanym zbiorze, które są
   niewiększe od danej wartości x *)
let below x t =
  let (l, b, _) = split x t in
  safe_add (count l) (if b then 1 else 0);;
