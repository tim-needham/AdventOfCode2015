module Day17

open System;
open System.IO;

let rec powerset (js : int list) : (int list list) =
    match js with
        | [] -> [[]]
        | k::ks -> List.collect (fun s -> [s; k::s]) (powerset ks);

let run (file : string) =
    let containers =  Seq.toList (File.ReadLines(file))
                        |> List.map (fun x -> Int32.Parse(x))
                        |> powerset
                        |> List.filter (fun x -> (List.sum x) = 150);

    printfn "Day 17, part 1: %d" containers.Length;

    let min = List.sortBy (fun x -> List.length x) containers
                |> List.head
                |> List.length;
   
    List.filter (fun x -> List.length x = min) containers
    |> List.length
    |> printfn "Day 17, part 2: %d";

