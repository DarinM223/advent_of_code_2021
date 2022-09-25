open Utils

let path = "inputs/day12/input.txt"

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let is_small i = Char.(code i.[0] >= code 'a')

let num_paths part graph =
  let rec go part visited acc i =
    let part =
      if StringSet.mem i visited then
        if i = "start" then 0 else if is_small i then part - 1 else part
      else part
    in
    if i = "end" then acc + 1
    else if part = 0 then acc
    else
      let visited = StringSet.add i visited in
      List.fold_left (go part visited) acc (StringMap.find i graph)
  in
  go part StringSet.empty 0 "start"

let insert_edge a b graph =
  graph
  |> StringMap.update a (fun n1 -> Some (b :: Option.value n1 ~default:[]))
  |> StringMap.update b (fun n2 -> Some (a :: Option.value n2 ~default:[]))

let parse =
  let rec go graph seq =
    match seq () with
    | Seq.Cons (line, seq) -> (
        match String.split_on_char '-' line with
        | [ a; b ] -> go (insert_edge a b graph) seq
        | _ -> failwith "Invalid line")
    | Seq.Nil -> graph
  in
  go StringMap.empty

let graph = parse @@ lines_seq @@ open_in path
let part1 = num_paths 1 graph
let part2 = num_paths 2 graph