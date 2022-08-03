open Utils

let path = "inputs/day5/input.txt"

module IntTupleMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let ( >> ) g f e = f (g e)

let[@tail_mod_cons] rec parse seq =
  match seq () with
  | Seq.Cons (line, seq) -> (
      let parsed =
        line
        |> Str.split (Str.regexp "->")
        |> List.map
             (String.split_on_char ','
             >> List.map (String.trim >> int_of_string))
      in
      match parsed with
      | [ [ a; b ]; [ c; d ] ] -> ((a, b), (c, d)) :: parse seq
      | p -> failwith @@ "Invalid parsed data: " ^ [%show: int list list] p)
  | Seq.Nil -> []

let is_straight_line ((x1, y1), (x2, y2)) = x1 = x2 || y1 = y2

let range i j =
  let rec go build i j = if i > j then build else go (i :: build) (i + 1) j in
  if i > j then go [] j i else List.rev (go [] i j)

let fill map ((x1, y1), (x2, y2)) =
  let go =
    List.fold_left (fun map point ->
        IntTupleMap.update point
          (fun e -> match e with None -> Some 1 | Some i -> Some (i + 1))
          map)
  in
  if x1 = x2 then go map @@ List.map (fun y -> (x1, y)) (range y1 y2)
  else if y1 = y2 then go map @@ List.map (fun x -> (x, y1)) (range x1 x2)
  else go map @@ List.combine (range x1 x2) (range y1 y2)

let part1 =
  let lines = lines_seq (open_in path) in
  let parsed = List.filter is_straight_line (parse lines) in
  let map = List.fold_left fill IntTupleMap.empty parsed in
  List.length @@ List.filter (fun (_, v) -> v > 1) @@ IntTupleMap.bindings map

let part2 =
  let parsed = parse @@ lines_seq @@ open_in path in
  let map = List.fold_left fill IntTupleMap.empty parsed in
  List.length @@ List.filter (fun (_, v) -> v > 1) @@ IntTupleMap.bindings map