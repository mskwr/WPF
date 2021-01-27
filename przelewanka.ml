(* Zadanie 6: Przelewanka *)
(* Autor: Michał Skwarek *)
(* Code Review: Jan Wojtach *)

(* Funkcja znajdująca największy wspólny dzielnik dwóch liczb *)
let rec nwd a b =
  if b = 0 then a
  else nwd (b) (a mod b);;

let przelewanka t =
  (* Usuwam z tablicy szklanki o pojemności zero - nie wpływają na wynik *)
  let tab =
    Array.of_list (List.filter (fun (x, _) -> x <> 0) (Array.to_list t))
  in
  (* Liczę NWD pojemności wszystkich szklanek *)
  let cap_nwd =
    Array.fold_left (fun a (x, _) -> nwd (min x a) (max x a)) 0 tab
  in
  (* Ilość szklanek o niezerowych pojemnościach *)
  let length = Array.length tab in
  (* Pożądany finalny stan wody w szklankach *)
  let final_cap = Array.map snd tab in
  (* Brak szklanek o niezerowych pojemnościach - nic nie trzeba robić *)
  if length = 0 then 0
  (* Dla każdej szklanki pożądana ilość wody musi być podzielna przez NWD
     pojemności wszystkich szklanek - jeśli nie, to nigdy nie uda się tej
     szklanki zapełnić *)
  else if not (Array.for_all (fun (_, y) -> y mod cap_nwd = 0) tab) then -1
  (* Przynajmniej jedna szklanka musi być docelowo pusta lub maksymalnie
     wypełniona - jeśli nie, to gdy wszystkie szklanki będą właściwie
     napełnione oprócz jednej, nie uda się osiągnąć pożądanego stanu *)
  else if not (Array.exists (fun (x, y) -> (x = y || y = 0)) tab) then -1
  else
    (* Licznik czynności potrzebnych do osiągnięcia danego poziomu *)
    let count = ref (-1) in
    (* Kolejka stanów możliwych do uzyskania *)
    let q = Queue.create () in
    (* Tablica hashująca ze stanami, do których można doprowadzić *)
    let ht = Hashtbl.create 1000000 in
    (* Funkcja dodająca dany, unikalny stan do tablicy haszującej *)
    let update s =
      (* Sprawdzenie czy danego stanu już nie ma w tablicy *)
      if not (Hashtbl.mem ht s) then
        begin
          Hashtbl.add ht s (!count + 1);
          Queue.push s q
        end
    in
    (* Funkcja napełniająca szklankę wodą z kranu lub wylewająca ją do zlewu
       dla zadanego indeksu i danego stanu *)
    let sink s i =
      let fill = Array.copy s in
      let empty = Array.copy s in
      (* Wypełnianie szklanki wodą z kranu *)
      if s.(i) <> fst tab.(i) then
        begin
          fill.(i) <- fst tab.(i);
          update fill
        end;
      (* Wylewanie całej wody z danej szklanki do zlewu *)
      if s.(i) <> 0 then
        begin
          empty.(i) <- 0;
          update empty
        end
    in
    (* Funkcja przelewająca wodę ze szklanki o indeksie "i" do szklanki o
       indeksie "j" w danym stanie *)
    let transfer s i j =
      let c = Array.copy s in
      (* Przypadek, gdy możliwe jest przelanie całej wody ze szklanki "i" *)
      if s.(j) + s.(i) <= fst tab.(j) then
        begin
          c.(i) <- 0;
          c.(j) <- s.(j) + s.(i);
          update c
        end
      (* Przypadek, gdy szklanka "j" zostaje cała wypełniona, a w szklance
         "i" pozostaje jeszcze pewna ilość wody *)
      else
        begin
          c.(i) <- s.(i) - (fst tab.(j) - s.(j));
          c.(j) <- fst tab.(j);
          update c
        end
    in
    (* Funkcja sprawdzająca czy pożądany stan jest w tablicy hashującej *)
    let find () =
      if Hashtbl.mem ht final_cap then
        begin
          Queue.clear q;
          true
        end
      else false
    in
    begin
      (* Dodanie stanu zerowego do tablicy hashującej *)
      update (Array.make length 0);
      while not (Queue.is_empty q) do
        (* Aktualnie rozpatrywany stan szklanek *)
        let s = Queue.pop q in
        if not (find ()) then
          begin
            count := Hashtbl.find ht s;
            (* Wykonanie wszystkich możliwych czynności dla danego stanu *)
            for i = 0 to (length - 1) do
              sink s i;
              for j = 0 to (length - 1) do
                if i <> j && s.(i) <> 0 then transfer s i j
              done
            done
          end
      done;
      (* Sprawdzenie, czy udało się uzyskać pożądany stan szklanek *)
      if Hashtbl.mem ht final_cap then Hashtbl.find ht final_cap
      else -1
    end;;
