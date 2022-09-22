open Utils

let path = "inputs/day4/input.txt"

module IntTupleMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type grid = {
  marked : bool IntTupleMap.t;
  lookup : (int * int) IntMap.t;
  unmarked : IntSet.t;
}

let pp_grid fmt grid =
  Format.fprintf fmt "{ marked = %s, lookup = %s, unmarked = %s }"
    ([%show: ((int * int) * bool) list] (IntTupleMap.bindings grid.marked))
    ([%show: (int * (int * int)) list] (IntMap.bindings grid.lookup))
    ([%show: int list] (IntSet.elements grid.unmarked))

let pp_grids fmt grids =
  List.iter (fun grid -> Format.fprintf fmt "%a" pp_grid grid) grids

let empty_grid =
  { marked = IntTupleMap.empty; lookup = IntMap.empty; unmarked = IntSet.empty }

let grid_row_size = 5
let grid_col_size = 5

let check_grid grid (row, col) =
  let rec check_cols col =
    col >= grid_col_size
    || (IntTupleMap.find (row, col) grid.marked && check_cols (col + 1))
  and check_rows row =
    row >= grid_row_size
    || (IntTupleMap.find (row, col) grid.marked && check_rows (row + 1))
  in
  check_cols 0 || check_rows 0

let mark_number grid num =
  match IntMap.find_opt num grid.lookup with
  | Some pos ->
      let grid =
        {
          grid with
          marked = IntTupleMap.add pos true grid.marked;
          unmarked = IntSet.remove num grid.unmarked;
        }
      in
      (grid, check_grid grid pos)
  | _ -> (grid, false)

let parse seq : int list * grid list =
  let numbers, seq =
    match seq () with
    | Seq.Cons (line, seq) ->
        let nums = line |> String.split_on_char ',' |> List.map int_of_string in
        (nums, seq)
    | Seq.Nil -> failwith "No more lines"
  in
  let update_grid grid row col num =
    {
      marked = IntTupleMap.add (row, col) false grid.marked;
      lookup = IntMap.add num (row, col) grid.lookup;
      unmarked = IntSet.add num grid.unmarked;
    }
  in
  let rec parse_grid ?(grid = empty_grid) ?(row = 0) seq =
    match seq () with
    | Seq.Cons ("", seq) -> (grid, seq)
    | Seq.Cons (line, seq) ->
        let nums =
          line |> String.split_on_char ' '
          |> List.filter (fun s -> String.length (String.trim s) <> 0)
          |> List.map int_of_string
        in
        let grid, _ =
          List.fold_left
            (fun (grid, col) num -> (update_grid grid row col num, col + 1))
            (grid, 0) nums
        in
        parse_grid ~grid ~row:(row + 1) seq
    | Seq.Nil -> (grid, seq)
  in
  let rec collect grids seq =
    if Seq.is_empty seq then grids
    else
      let grid, seq = parse_grid seq in
      collect (grid :: grids) seq
  in
  (numbers, collect [] (Seq.drop 1 seq))

exception GridWon of grid * int

let first_score bingo grids =
  try
    let mark_grids grids num =
      List.map
        (fun grid ->
          let grid, won = mark_number grid num in
          if won then raise (GridWon (grid, num)) else grid)
        grids
    in
    let _ = List.fold_left mark_grids grids bingo in
    failwith @@ Format.asprintf "Bingo game has no winner: %a\n" pp_grids grids
  with GridWon (grid, num) -> IntSet.fold ( + ) grid.unmarked 0 * num

let last_score bingo grids =
  let last_grid = ref (List.hd grids) in
  let last_num = ref 0 in
  let[@tail_mod_cons] rec mark_grids grids num =
    match (grids, num) with
    | grid :: rest, num ->
        let grid, won = mark_number grid num in
        if won then (
          last_grid := grid;
          last_num := num;
          mark_grids rest num)
        else grid :: mark_grids rest num
    | [], _ -> []
  in
  let _ = List.fold_left mark_grids grids bingo in
  IntSet.fold ( + ) !last_grid.unmarked 0 * !last_num

let part1 =
  let bingo, grids = parse @@ lines_seq @@ open_in path in
  first_score bingo grids

let part2 =
  let bingo, grids = parse @@ lines_seq @@ open_in path in
  last_score bingo grids

let%test "part1" = part1 = 54275
let%test "part2" = part2 = 13158