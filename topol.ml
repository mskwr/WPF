(* Zadanie 5: Sortowanie topologiczne *)
(* Autor: Michał Skwarek *)
(* Code Review: Sylwia Szunejko *)

(* Wyjątek podnoszony, gdy zależności są cykliczne *)
exception Cykliczne;;

(* Funkcja tworząca graf na podstawie danej listy *)
let graph lst =
  let aux acc (x, list) =
    (* Trzymana na mapie trójka to kolejno: czy wierzchołek był odwiedzony,
       lista sąsiadów, czy wierzchołek jest na liście wynikowej *)
    PMap.add x (false, list, false) acc
  in
  List.fold_left aux (PMap.create compare) lst;;

(* Funkcja rozszerzająca graf skierowany bez cykli do porządku liniowego *)
let topol l =
  (* Lista wynikowa *)
  let topo = ref []
  and g = ref (graph l)
  in
  (* Przejście po wszystkich elementach grafu za pomocą DFSa  *)
  let rec dfs x =
    (* Funkcja sprawdzająca, czy zadana wartość znajduje się na mapie *)
    let check =
      if PMap.mem x !g then
        try PMap.find x !g with Not_found -> (true, [], true)
      else
        (false, [], false)
    in
    match check with
    (* Wierzchołek odwiedzony i znajdujący się już na liście wynikowej *)
    | (true, _, true) -> ()
    (* Wierzchołek odwiedzony i nie znajdujący się na liście wynikowej,
       a więc rozpatrywany ponownie w wyniku zacyklenia *)
    | (true, _, false) -> raise Cykliczne
    (* Wierzchołek nieodwiedzony *)
    | (_, lst, _) ->
      (* Oznaczenie, że wierzchołek został odwiedzony *)
      g := PMap.add x (true, lst, false) !g;
      List.iter dfs lst;
      (* Wpisanie wierzchołka na listę wynikową *)
      g := PMap.add x (true, lst, true) !g;
      topo := x::!topo;
  in
  List.iter (function (x, _) -> dfs x) l;
  !topo;;
