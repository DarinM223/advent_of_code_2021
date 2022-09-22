open Utils

let path = "inputs/day1/input.txt"

let part1 =
  let lines = lines_seq (open_in path) in
  let prev, lines =
    match lines () with
    | Seq.Cons (line, lines) -> (ref (int_of_string line), lines)
    | Seq.Nil -> failwith "Can't read first line"
  in
  let count = ref 0 in
  Seq.iter
    (fun line ->
      let curr = int_of_string line in
      if curr > !prev then count := !count + 1;
      prev := curr)
    lines;
  !count

let part2 =
  let lines = lines_seq (open_in path) in
  let window, lines =
    ( lines |> Seq.take 3 |> Seq.map int_of_string |> seq_to_list |> ref,
      Seq.drop 3 lines )
  in
  let count = ref 0 in
  Seq.iter
    (fun line ->
      let prev_sum = List.fold_left ( + ) 0 !window in
      window := List.tl !window @ [ int_of_string line ];
      let curr_sum = List.fold_left ( + ) 0 !window in
      if curr_sum > prev_sum then count := !count + 1)
    lines;
  !count

let%test "part1" = part1 = 1665
let%test "part2" = part2 = 1702