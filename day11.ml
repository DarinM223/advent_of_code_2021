open Utils

let path = "inputs/day11/input.txt"

module IntTupleMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let incr_pos pos =
  match pos with row, 9 -> (row + 1, 0) | row, col -> (row, col + 1)

let parse_grid =
  let pos = ref (0, -1) in
  Seq.fold_left
    (String.fold_left (fun map ch ->
         pos := incr_pos !pos;
         IntTupleMap.add !pos Char.(code ch - code '0') map))
    IntTupleMap.empty

let adjs (row, col) =
  [ (row - 1, col - 1); (row - 1, col); (row - 1, col + 1) ]
  @ [ (row, col - 1); (row, col + 1) ]
  @ [ (row + 1, col - 1); (row + 1, col); (row + 1, col + 1) ]

let step grid =
  let flashed = Hashtbl.create 100 in
  let rec handle_octopus grid pos =
    match IntTupleMap.find_opt pos grid with
    | Some energy when not (Hashtbl.mem flashed pos) ->
        let energy = energy + 1 in
        let grid = IntTupleMap.add pos energy grid in
        if energy > 9 then (
          Hashtbl.add flashed pos ();
          List.fold_left handle_octopus grid (adjs pos))
        else grid
    | _ -> grid
  in
  let grid =
    IntTupleMap.fold (fun pos _ grid -> handle_octopus grid pos) grid grid
  in
  ( Hashtbl.length flashed,
    Hashtbl.fold (fun pos _ grid -> IntTupleMap.add pos 0 grid) flashed grid )

let part1 =
  let grid = parse_grid @@ lines_seq @@ open_in path in
  let rec run_steps acc grid = function
    | 0 -> acc
    | n ->
        let flashed, grid = step grid in
        run_steps (acc + flashed) grid (n - 1)
  in
  run_steps 0 grid 100

let part2 =
  let grid = parse_grid @@ lines_seq @@ open_in path in
  let rec run steps grid =
    let flashed, grid = step grid in
    if flashed = 100 then steps + 1 else run (steps + 1) grid
  in
  run 0 grid