module Day9

open System;
open System.IO;

let square (m : int) : int list list =
    [for p in 1..m -> [for q in 1..m -> 0]];

let update (x : int) (y : int) (v : int) (m : int list list) : int list list =
    [for p in 0..m.Length-1 -> [for q in 0..m.[p].Length-1 -> if p = x && q = y then v else m.[p].[q]]];

let rec find (l : string) (ls : (string * int) list) : int =
    match ls with
        | [] -> 0
        | x::xs ->  let (a, b) = x;
                    if a = l then b else find l xs;

let rec build (gs : (string * string * int) list) (ls : (string * int) list) (m : int list list) : int list list =
    match gs with
        | [] -> m
        | x::xs ->  let (a, b, c) = x;
                    let p = find a ls;
                    let q = find b ls;
                    build xs ls (update q p c (update p q c m));

let rec index (g : string) (n : int) (ls : (string * int) list) : (string * int) list =
    match ls with
        | [] -> [(g, n)]
        | x::xs ->  let (a, b) = x
                    if g = a then ls else x :: index g (n+1) xs;

let parse (g : string) : (string * string * int) =
    match g.Split(' ') with
        | [|a; "to"; b; "="; c|] -> (a, b, Int32.Parse(c))

let rec remove (n : int) (ns : int list) : int list =
    match ns with
        | [] -> []
        | x::xs -> if x = n then xs else x :: (remove n xs);

let rec trip (s : int) (ns : int list) (m : int list list) (f : (int list) -> int) : int =
    match ns with
        | [] -> 0
        | [x] -> m.[s].[x]
        | x::xs -> f [for p in ns -> m.[s].[p] + (trip p (remove p ns) m f)];

let run (file : string) =
    let distances = Seq.toList (File.ReadLines(file))
                    |> List.map (fun x -> parse x);
    let labels = List.fold (fun l (a, b, c) -> index b 0 (index a 0 l)) [] distances;
    let graph = square labels.Length
                |> build distances labels;
    let nodes = [for p in 0..labels.Length-1 -> p];

    List.map (fun x -> trip x (remove x nodes) graph List.min) nodes
    |> List.min
    |> printfn "Day 9, part 1: %d";

    List.map (fun x -> trip x (remove x nodes) graph List.max) nodes
    |> List.max
    |> printfn "Day 9, part 2: %d";
