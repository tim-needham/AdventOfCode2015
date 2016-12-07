module Day6

open System;
open System.Collections.Generic;
open System.IO;

let coords (c : string) : (int * int) =
    match c.Split(',') with
        | [| p; q |] -> (Int32.Parse(p), Int32.Parse(q));

type Command =
    | On of int * int * int * int
    | Off of int * int * int * int
    | Toggle of int * int * int * int

let parse (s : string) : Command =
    match s.Split(' ') with
        | [| "turn"; "on"; x; "through"; y |] ->    let (a, b) = coords x;
                                                    let (c, d) = coords y;
                                                    On (a, b, c, d);
        | [| "turn"; "off"; x; "through"; y |] ->   let (a, b) = coords x;
                                                    let (c, d) = coords y;
                                                    Off (a, b, c, d);
        | [| "toggle"; x; "through"; y |] ->    let (a, b) = coords x;
                                                let (c, d) = coords y;
                                                Toggle (a, b, c, d);
        
let part1 (c : Command) (ls : int [] []) : unit =
    match c with
        | On (a, b, c, d) -> for p in a..c do
                                for q in b..d do
                                    Array.set ls.[p] q 1;
        | Off (a, b, c, d) ->  for p in a..c do
                                for q in b..d do
                                    Array.set ls.[p] q 0;
        | Toggle (a, b, c, d) ->  for p in a..c do
                                    for q in b..d do
                                        Array.set ls.[p] q (1 - ls.[p].[q]);

let part2 (c : Command) (ls : int [] []) : unit =
    match c with
        | On (a, b, c, d) -> for p in a..c do
                                for q in b..d do
                                    Array.set ls.[p] q (ls.[p].[q] + 1);
        | Off (a, b, c, d) ->  for p in a..c do
                                for q in b..d do
                                    Array.set ls.[p] q (max 0 (ls.[p].[q] - 1));
        | Toggle (a, b, c, d) ->  for p in a..c do
                                    for q in b..d do
                                        Array.set ls.[p] q (ls.[p].[q] + 2);

let run (file : string) =
    let commands = File.ReadLines(file)
                    |> Seq.toList
                    |> List.map (fun s -> parse s);

    let lights = [| for p in 0..999 -> [| for q in 0..999 -> 0 |]|];
    List.iter (fun x -> part1 x lights) commands;

    Array.sumBy (fun x -> Array.sum x) lights
    |> printfn "Day 6, part 1: %d";

    let lights2 = [| for p in 0..999 -> [| for q in 0..999 -> 0 |]|];
    List.iter (fun x -> part2 x lights2) commands;
    
    Array.sumBy (fun x -> Array.sum x) lights2
    |> printfn "Day 6, part 1: %d";

