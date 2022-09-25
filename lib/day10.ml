open Utils

let path = "inputs/day10/input.txt"

let closing_char = function
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | '<' -> '>'
  | _ -> failwith "Invalid char"

let do_stack stack ch =
  match (stack, ch) with
  | _, '(' | _, '[' | _, '{' | _, '<' -> Ok (closing_char ch :: stack)
  | ch' :: stack, _ when ch = ch' -> Ok stack
  | _ -> Error ch

exception FoundError of char

let corruption line =
  let stack = ref [] in
  try
    String.iter
      (fun ch ->
        match do_stack !stack ch with
        | Ok stack' -> stack := stack'
        | Error ch -> raise (FoundError ch))
      line;
    None
  with FoundError ch -> Some ch

let error_score = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | ch -> failwith @@ Printf.sprintf "Invalid char %c" ch

let complete_score = function
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | ch -> failwith @@ Printf.sprintf "Invalid char %c" ch

let complete_string line =
  let go stack ch = Result.get_ok (do_stack stack ch) in
  let stack = Array.of_list @@ String.fold_left go [] line in
  String.init (Array.length stack) (fun i -> stack.(i))

let part1 =
  let lines = List.of_seq @@ lines_seq @@ open_in path in
  lines |> List.filter_map corruption
  |> List.fold_left (fun acc ch -> acc + error_score ch) 0

let part2 =
  let lines = List.of_seq @@ lines_seq @@ open_in path in
  let incomplete = List.filter (fun l -> Option.is_none (corruption l)) lines in
  let scores =
    incomplete
    |> List.map (fun l ->
           String.fold_left
             (fun acc ch -> (acc * 5) + complete_score ch)
             0 (complete_string l))
    |> Array.of_list
  in
  Array.fast_sort compare scores;
  scores.(Array.length scores / 2)