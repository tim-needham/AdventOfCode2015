module Day5

open System;
open System.IO;

let rec vowels (n : int) (cs : char list) : bool =
    match cs with
        | [] -> false
        | x::xs -> if x = 'a' || x = 'e' || x = 'i' || x = 'o' || x = 'u' then
                        if n = 2 then true else vowels (n+1) xs
                   else
                        vowels n xs;

let rec doubles (cs : char list) : bool =
    match cs with
        | [] -> false
        | [x] -> false
        | x::xs ->  if x = List.head xs then true else doubles xs;

let rec clean (cs : char list) : bool =
    match cs with
        | [] -> true
        | [x] -> true
        | x::xs -> match x with
                    | 'a' -> if List.head xs = 'b' then false else clean xs;
                    | 'c' -> if List.head xs = 'd' then false else clean xs;
                    | 'p' -> if List.head xs = 'q' then false else clean xs;
                    | 'x' -> if List.head xs = 'y' then false else clean xs;
                    | _ -> clean xs;

let rec checkPairs (p : char) (q : char) (cs : char list) : bool =
    match cs with
        | [] -> false;
        | [x] -> false;
        | x::xs -> if checkPair p q cs then true else checkPairs q x xs;
and checkPair (p : char) (q : char) (cs : char list) : bool =
    match cs with
        | [] -> false
        | [x] -> false
        | x::xs -> if p = x && q = List.head xs then true else checkPair p q xs;

let pairs (cs : char list) : bool =
    match cs with
        | [] -> false
        | [x] -> false
        | x::xs -> checkPairs x (List.head xs) (List.tail xs);

let rec separated (cs : char list) : bool =
    match cs with
        | [] -> false
        | [x] -> false
        | [x; y] -> false
        | x::xs ->  if x = List.head (List.tail xs) then
                        true
                    else
                        separated xs

let run (file : string) =
    let strings = Seq.toList (File.ReadLines(file))
                    |> List.map (fun x -> Seq.toList (x.ToLower()));

    strings
    |> List.filter (fun x -> vowels 0 x)
    |> List.filter (fun x -> doubles x)
    |> List.filter (fun x -> clean x)
    |> List.length
    |> printfn "Day 5, part 1: %d";

    strings
    |> List.filter (fun x -> pairs x)
    |> List.filter (fun x -> separated x)
    |> List.length
    |> printfn "Day 5, part 2: %d";
