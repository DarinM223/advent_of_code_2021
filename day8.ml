open Utils

let path = "inputs/day8/input.txt"
let ( << ) f g e = f (g e)

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

let sort s =
  let n = String.length s in
  let a = Array.init n (fun i -> s.[i]) in
  Array.fast_sort Char.compare a;
  String.init n (fun i -> a.(i))

let valid_digits =
  Hashtbl.of_seq
    (List.to_seq
       [
         ("abcefg", 0);
         ("cf", 1);
         ("acdeg", 2);
         ("acdfg", 3);
         ("bcdf", 4);
         ("abdfg", 5);
         ("abdefg", 6);
         ("acf", 7);
         ("abcdefg", 8);
         ("abcdfg", 9);
       ])

let convert code =
  sort << String.map (fun ch -> String.get code (Char.code ch - Char.code 'a'))

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
      let digits = List.map (Hashtbl.find valid_digits << convert code) out in
      acc + int_of_digits digits)
    0 entries