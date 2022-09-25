let rec lines_seq chan =
  match In_channel.input_line chan with
  | Some line -> Seq.cons line (lines_seq chan)
  | None -> Seq.empty

let rec seq_to_list seq =
  match seq () with
  | Seq.Cons (a, rest) -> a :: seq_to_list rest
  | Seq.Nil -> []

let seq_next seq =
  match seq () with
  | Seq.Cons (a, rest) -> (a, rest)
  | Seq.Nil -> failwith "Empty seq"