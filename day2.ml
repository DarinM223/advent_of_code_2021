open Utils

let path = "inputs/day2/input.txt"

let part1 =
  let update (pos, depth) direction amount =
    match direction with
    | "forward" -> (pos + amount, depth)
    | "up" -> (pos, depth - amount)
    | "down" -> (pos, depth + amount)
    | _ -> failwith "Invalid direction"
  in
  let lines = lines_seq (open_in path) in
  let ctx = ref (0, 0) in
  Seq.iter
    (fun line ->
      match String.split_on_char ' ' line with
      | [ direction; amount ] ->
          ctx := update !ctx direction (int_of_string amount)
      | _ -> failwith "Invalid line")
    lines;
  fst !ctx * snd !ctx

let part2 =
  let update (pos, depth, aim) direction amount =
    match direction with
    | "forward" -> (pos + amount, depth + (aim * amount), aim)
    | "up" -> (pos, depth, aim - amount)
    | "down" -> (pos, depth, aim + amount)
    | _ -> failwith "Invalid direction"
  in
  let lines = lines_seq (open_in path) in
  let ctx = ref (0, 0, 0) in
  Seq.iter
    (fun line ->
      match String.split_on_char ' ' line with
      | [ direction; amount ] ->
          ctx := update !ctx direction (int_of_string amount)
      | _ -> failwith "Invalid line")
    lines;
  let pos, depth, _ = !ctx in
  pos * depth

let%test "part1" = part1 = 2117664
let%test "part2" = part2 = 2073416724