module Day8

open System;
open System.Globalization;
open System.IO;

let toHex (c : char) (d : char) : char list =
    Char.ConvertFromUtf32(
        Int32.Parse(
            String.Concat (c :: [d]), 
            NumberStyles.HexNumber
        )
    ) |> Seq.toList;
    
let rec clump (c : char) (cs : char list) (os : char list) : char list =
    match cs with
        | [] -> os @ [c]
        | [x] -> match c with
                    | '\\' -> match x with
                                | '\\' -> os @ ['\\']
                                | '"' -> os @ ['"']
                                | _ -> os @ [c] @ [x]
                    | _ -> os @ [c] @ [x]
        | x::xs -> match c with
                    | '\\' -> match x with 
                                | '\\' -> clump (List.head xs) (List.tail xs) (os @ ['\\'])
                                | '"' -> clump (List.head xs) (List.tail xs) (os @ ['"'])
                                | 'x' -> let e = List.head xs;
                                         let es = List.tail xs;
                                         let f = List.head es;
                                         let fs = List.tail es;
                                         match fs with
                                            | [] -> os @ (toHex e f)
                                            | y::ys -> clump y ys (os @ (toHex e f))
                                | _ -> clump x xs (os @ [c])
                    | _ -> clump x xs (os @ [c]);

let escape (c : char) : (char list) =
    match c with
        | '\\' -> '\\' :: ['\\']
        | '"' -> '\\' :: [c]
        | _ -> [c];

let rec plump (os : char list) (cs : char list) : char list =
    match cs with
        | [] -> os
        | x::xs -> plump (os @ (escape x)) xs;

let part1 (s : string) : string =
    let cs = s
            |> (fun s -> s.Substring(1, s.Length - 2))
            |> Seq.toList;

    clump (List.head cs) (List.tail cs) []
    |> String.Concat;

let part2 (s : string) : string =
    s
    |> Seq.toList
    |> plump []
    |> String.Concat
    |> (fun x -> "\"" + x + "\"");

let run (file : string) =
    let strings = Seq.toList (File.ReadLines(file));
    let length = List.fold (fun ac (s : string) -> ac + s.Length) 0 strings;

    let slength = List.map (fun x -> part1 x) strings
                    |> List.fold (fun ac s -> ac + s.Length) 0;
    printfn "Day 8, part 1: %d" (length - slength)

    let plength = List.map(fun x -> part2 x) strings
                    |> List.fold (fun ac s -> ac + s.Length) 0;
    printfn "Day 8, part 2: %d" (plength - length)