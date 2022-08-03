open Utils

let input = "inputs/day6/input.txt"

module IntMap = Map.Make (Int)

let cycle map =
  let add_pop life pop map =
    IntMap.update life
      (fun pop' -> Some (Option.value pop' ~default:0 + pop))
      map
  in
  let update map = function
    | 0, pop -> map |> add_pop 6 pop |> add_pop 8 pop
    | life, pop -> add_pop (life - 1) pop map
  in
  List.fold_left update IntMap.empty (IntMap.bindings map)

let rec run_cycles map = function
  | 0 -> map
  | n -> run_cycles (cycle map) (n - 1)

let fish =
  let line, _ = Option.get @@ Seq.uncons @@ lines_seq @@ open_in input in
  let nums = line |> String.split_on_char ',' |> List.map int_of_string in
  List.fold_left
    (fun map num ->
      IntMap.update num (fun pop -> Some (Option.value pop ~default:0 + 1)) map)
    IntMap.empty nums

let part1 = IntMap.fold (fun _ -> ( + )) (run_cycles fish 80) 0
let part2 = IntMap.fold (fun _ -> ( + )) (run_cycles fish 256) 0