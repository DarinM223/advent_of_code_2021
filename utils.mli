val lines_seq : in_channel -> string Seq.t
val seq_to_list : 'a Seq.t -> 'a list
val seq_next : (unit -> 'a Seq.node) -> 'a * 'a Seq.t