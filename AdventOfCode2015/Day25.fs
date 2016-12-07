module Day25

open System;
open System.IO;

let rec traverse (s : int64) (r : int) (c : int) : int64 =
    match r, c with
        | 1, 1 -> s;
        | x, 1 -> traverse ((252533L * s) % 33554393L) 1 (x-1);
        | x, y -> traverse ((252533L * s) % 33554393L) (x+1) (y-1);

let run (file : string) =
    let lines = Seq.toList (File.ReadAllLines(file));
    let s = lines |> Seq.head |> Int64.Parse;
    let r = lines |> Seq.skip 1 |> Seq.head |> Int32.Parse;
    let c = lines |> Seq.skip 2 |> Seq.head |> Int32.Parse;

    traverse s r c
    |> printfn "Day 25, part 1: %d";