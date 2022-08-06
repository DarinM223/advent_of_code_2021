open Utils

let path = "inputs/day9/input.txt"

let[@tail_mod_cons] rec parse seq =
  match seq () with
  | Seq.Cons (line, seq) ->
      let num c = Char.code c - Char.code '0' in
      Array.init (String.length line) (fun i -> num line.[i]) :: parse seq
  | Seq.Nil -> []

let grid_index row col grid =
  if
    row < 0
    || row >= Array.length grid
    || col < 0
    || col >= Array.length grid.(0)
  then None
  else Some grid.(row).(col)

let adjs row col =
  [ (row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1) ]

let is_low_point row col grid =
  let value = grid.(row).(col) in
  List.for_all
    (fun (row, col) ->
      value < Option.value (grid_index row col grid) ~default:Int.max_int)
    (adjs row col)

let basin_size row col grid =
  let counted = Hashtbl.create 105 in
  let rec check row col prev_value grid =
    match grid_index row col grid with
    | None | Some 9 -> ()
    | Some value when value < prev_value || Hashtbl.mem counted (row, col) -> ()
    | Some value ->
        Hashtbl.add counted (row, col) ();
        check_adjs row col value
  and check_adjs row col value =
    List.fold_left
      (fun () (row, col) -> check row col value grid)
      () (adjs row col)
  in
  check_adjs row col grid.(row).(col);
  Hashtbl.length counted + 1

let part1 =
  let grid = Array.of_list @@ parse @@ lines_seq @@ open_in path in
  let sum = ref 0 in
  for row = 0 to Array.length grid - 1 do
    for col = 0 to Array.length grid.(row) - 1 do
      if is_low_point row col grid then sum := !sum + grid.(row).(col) + 1
    done
  done;
  !sum

let part2 =
  let grid = Array.of_list @@ parse @@ lines_seq @@ open_in path in
  let basins = ref [] in
  for row = 0 to Array.length grid - 1 do
    for col = 0 to Array.length grid.(row) - 1 do
      if is_low_point row col grid then
        basins := basin_size row col grid :: !basins
    done
  done;
  match List.rev (List.sort compare !basins) with
  | a :: b :: c :: _ -> a * b * c
  | _ -> failwith "There are less than three basins"