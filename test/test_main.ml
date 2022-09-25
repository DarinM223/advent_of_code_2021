open Alcotest
open Advent

let _ =
  run "Tests"
    [
      ( "Day1",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day1.part1 1665);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day1.part2 1702);
        ] );
      ( "Day2",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day2.part1 2117664);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day2.part2 2073416724);
        ] );
      ( "Day3",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day3.part1 1071734);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day3.part2 6124992);
        ] );
      ( "Day4",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day4.part1 54275);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day4.part2 13158);
        ] );
      ( "Day5",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day5.part1 6548);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day5.part2 19663);
        ] );
      ( "Day6",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day6.part1 366057);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day6.part2 1653559299811);
        ] );
      ( "Day7",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day7.part1 344138);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day7.part2 94862124);
        ] );
      ( "Day8",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day8.part1 512);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day8.part2 1091165);
        ] );
      ( "Day9",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day9.part1 600);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day9.part2 987840);
        ] );
      ( "Day10",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day10.part1 315693);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day10.part2 1870887234);
        ] );
      ( "Day11",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day11.part1 1644);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day11.part2 229);
        ] );
      ( "Day12",
        [
          test_case "part1" `Quick (fun () ->
              (check int) "same int" Day12.part1 5157);
          test_case "part2" `Quick (fun () ->
              (check int) "same int" Day12.part2 144309);
        ] );
    ]
