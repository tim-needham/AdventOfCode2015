module Day3

open System.IO;

let move (p : int * int) (m : char) : (int * int) =
    let (x, y) = p;
    match m with
        | '<' -> (x - 1, y)
        | '>' -> (x + 1, y)
        | '^' -> (x, y + 1)
        | 'v' -> (x, y - 1)
        | _ -> p;

let run (file : string) =
    let moves = (Seq.toList (File.ReadLines(file))).[0]
                |> Seq.toList;

    List.fold (fun a x -> (move (List.head a) x) :: a) [(0, 0)] moves
    |> Seq.distinct
    |> Seq.length
    |> printfn "Day 3, part 1: %d";

    List.fold (fun (a, b) x -> if List.length a = List.length b then ((move (List.head a) x) :: a, b) else (a, (move (List.head b) x) :: b)) ([(0, 0)], [(0, 0)]) moves
    ||> (@)
    |> Seq.distinct
    |> Seq.length;
    |> printfn "Day 3, part 2: %d";