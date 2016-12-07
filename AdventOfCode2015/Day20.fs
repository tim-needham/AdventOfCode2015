module Day20

open System;
open System.IO;

let sieve (t : int) (d : int) (b : int -> int) : int =
    let s = Array.zeroCreate ((t/d) + 1); 

    for i in 1 .. t/d do
        s.[i] <- d;

    for i in 2 .. t/d do
        for j in 1 .. b ((t/d)/i) do
            s.[i*j] <- s.[i*j] + (i*d);

    Array.findIndex (fun x -> x >= t) s

let run (file : string) =
    let input = (Seq.toList (File.ReadAllLines(file))).[0]
                |> Int32.Parse;

    sieve input 10 (fun x -> x)
    |> printfn "Day 20, part 1: %d";

    sieve input 11 (fun x -> min x 50)
    |> printfn "Day 20, part 2: %d";