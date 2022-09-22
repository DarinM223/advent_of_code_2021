open Utils

let path = "inputs/day8/input.txt"

let[@tail_mod_cons] rec parse seq =
  let split str = String.split_on_char ' ' (String.trim str) in
  match seq () with
  | Seq.Cons (line, seq) -> (
      match String.split_on_char '|' line with
      | [ pattern; output ] -> (split pattern, split output) :: parse seq
      | _ -> failwith "Invalid line: must have | delimiter")
  | Seq.Nil -> []

let is_easy signal =
  let len = String.length signal in
  len = 2 || len = 3 || len = 4 || len = 7

let permutations l =
  let rec go path build = function
    | [] -> path :: build
    | l ->
        List.fold_left
          (fun build n ->
            go (n :: path) build (List.filter (fun e -> e <> n) l))
          build l
  in
  go [] [] l

let string_permutations s =
  List.map (fun l -> String.of_seq (List.to_seq l))
  @@ permutations @@ List.of_seq @@ String.to_seq s

let code_permutations = string_permutations "abcdefg"

let valid_digits =
  Hashtbl.of_seq @@ List.to_seq
  @@ List.concat
       [
         List.map (fun s -> (s, 0)) @@ string_permutations "abcefg";
         List.map (fun s -> (s, 1)) @@ string_permutations "cf";
         List.map (fun s -> (s, 2)) @@ string_permutations "acdeg";
         List.map (fun s -> (s, 3)) @@ string_permutations "acdfg";
         List.map (fun s -> (s, 4)) @@ string_permutations "bcdf";
         List.map (fun s -> (s, 5)) @@ string_permutations "abdfg";
         List.map (fun s -> (s, 6)) @@ string_permutations "abdefg";
         List.map (fun s -> (s, 7)) @@ string_permutations "acf";
         List.map (fun s -> (s, 8)) @@ string_permutations "abcdefg";
         List.map (fun s -> (s, 9)) @@ string_permutations "abcdfg";
       ]

let convert code = String.map (fun ch -> code.[Char.code ch - Char.code 'a'])

let find_code pattern =
  let rec find_first = function
    | code :: rest ->
        let pattern = List.map (convert code) pattern in
        if List.for_all (Hashtbl.mem valid_digits) pattern then code
        else find_first rest
    | _ -> failwith "No code matches"
  in
  find_first code_permutations

let int_of_digits = List.fold_left (fun acc digit -> (acc * 10) + digit) 0

let part1 =
  let entries = parse @@ lines_seq @@ open_in path in
  List.fold_left
    (fun acc (_, output) -> acc + List.length (List.filter is_easy output))
    0 entries

let part2 =
  let entries = parse @@ lines_seq @@ open_in path in
  List.fold_left
    (fun acc (pat, out) ->
      let code = find_code pat in
      let digits =
        List.map (fun s -> Hashtbl.find valid_digits (convert code s)) out
      in
      acc + int_of_digits digits)
    0 entries

let%test "part1" = part1 = 512
let%test "part2" = part2 = 1091165