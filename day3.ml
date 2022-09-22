open Utils

module type CONVERT = sig
  type a
  type b

  val convert : a -> b
end

module CharToInt = struct
  type a = char
  type b = int

  let convert ch = Char.(code ch - code '0')
end

module Identity (Type : sig
  type t
end) =
struct
  type a = Type.t
  type b = Type.t

  let convert a = a
end

module type FOLDABLE = sig
  type elem
  type 'a t

  val fold_left : ('a -> elem -> 'a) -> 'a -> elem t -> 'a
  val length : 'a t -> int
end

module ArrayFoldable (Type : sig
  type t
end) =
struct
  type elem = Type.t
  type 'a t = 'a array

  let fold_left = Array.fold_left
  let length = Array.length
end

module StringFoldable = struct
  type elem = char
  type 'a t = string

  let fold_left = String.fold_left
  let length = String.length
end

module ToBinary
    (Foldable : FOLDABLE)
    (Convert : CONVERT with type a = Foldable.elem and type b = int) =
struct
  type a = Foldable.elem Foldable.t
  type b = Convert.b

  let convert (f : a) : b =
    fst
    @@ Foldable.fold_left
         (fun (build, i) e ->
           (build + Int.shift_left (Convert.convert e) i, i - 1))
         (0, Foldable.length f - 1)
         f
end

module IntArrayToBinary = ToBinary (ArrayFoldable (Int)) (Identity (Int))
module StringToBinary = ToBinary (StringFoldable) (CharToInt)

let path = "inputs/day3/input.txt"

let part1 =
  let lines = lines_seq (open_in path) in
  let bit_length = 12 in
  let num_zeros = Array.make bit_length 0 in
  let num_ones = Array.make bit_length 0 in
  Seq.iter
    (fun line ->
      String.iteri
        (fun i ch ->
          match ch with
          | '1' -> num_ones.(i) <- num_ones.(i) + 1
          | '0' -> num_zeros.(i) <- num_zeros.(i) + 1
          | _ -> failwith "Invalid digit")
        line)
    lines;
  let gamma = Array.make bit_length 0 in
  let epsilon = Array.make bit_length 0 in
  for i = 0 to bit_length - 1 do
    gamma.(i) <- (if num_zeros.(i) > num_ones.(i) then 0 else 1);
    epsilon.(i) <- (if num_zeros.(i) > num_ones.(i) then 1 else 0)
  done;
  IntArrayToBinary.(convert gamma * convert epsilon)

let part2 =
  let lines = lines_seq (open_in path) in
  let numbers = ref [] in
  Seq.iter (fun line -> numbers := line :: !numbers) lines;
  let rec stage keep_zeroes i = function
    | [ bits ] -> bits
    | l ->
        let zs, os =
          List.fold_left
            (fun (zs, os) s ->
              if s.[i] = '0' then (zs + 1, os) else (zs, os + 1))
            (0, 0) l
        in
        let l =
          if keep_zeroes zs os then List.filter (fun s -> s.[i] = '0') l
          else List.filter (fun s -> s.[i] = '1') l
        in
        stage keep_zeroes (i + 1) l
  in
  let oxygen = stage ( > ) 0 !numbers in
  let co2 = stage ( <= ) 0 !numbers in
  StringToBinary.(convert oxygen * convert co2)

let%test "part1" = part1 = 1071734
let%test "part2" = part2 = 6124992