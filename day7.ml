open Utils

let path = "inputs/day7/input.txt"

let median arr =
  let rec go i j =
    if i > j then (arr.(i) + arr.(j)) / 2
    else if i = j then arr.(i)
    else go (i + 1) (j - 1)
  in
  go 0 (Array.length arr - 1)

let part1 =
  let line, _ = Option.get @@ Seq.uncons @@ lines_seq @@ open_in path in
  let nums =
    line |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list
  in
  Array.fast_sort compare nums;
  let best_pos = median nums in
  Array.fold_left (fun acc n -> acc + abs (n - best_pos)) 0 nums

let part2 =
  let line, _ = Option.get @@ Seq.uncons @@ lines_seq @@ open_in path in
  let nums = line |> String.split_on_char ',' |> List.map int_of_string in
  let sum = List.fold_left ( + ) 0 nums in
  let mean =
    int_of_float
    @@ Float.round (float_of_int sum /. float_of_int (List.length nums))
  in
  let fuel_cost =
    let rec go acc i = if i = 0 then acc else go (acc + i) (i - 1) in
    go 0
  in
  let total_fuel_cost pos =
    List.fold_left (fun acc n -> acc + fuel_cost (abs (n - pos))) 0
  in
  (* also check mean - 1 and mean + 1 since they might be more efficient *)
  min
    (total_fuel_cost mean nums)
    (min (total_fuel_cost (mean - 1) nums) (total_fuel_cost (mean + 1) nums))