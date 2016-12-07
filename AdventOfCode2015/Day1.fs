module Day1

open System.IO;

let move (m : char) : int =
    match m with
        | '(' -> 1
        | ')' -> -1
        | _ -> 0;

let run (file : string) =
    let moves = (Seq.toList (File.ReadLines(file))).[0]
                |> Seq.toList;

    List.fold (fun a c -> a + (move c)) 0 moves
    |> printfn "Day 1, part 1: %d";

    List.scan (fun a c -> a + (move c)) 0 moves
    |> List.tryFindIndex (fun x -> x = -1)
    |> (fun x -> match x with
                    | Some x -> printfn "Day 1, part 2: %d" x);