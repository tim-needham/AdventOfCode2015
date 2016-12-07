module Day18

open System;
open System.IO;

let rec parse (c : char list) : int list =
    match c with
        | [] -> []
        | x::xs ->  match x with
                        | '#' -> 1 :: (parse xs)
                        | '.' -> 0 :: (parse xs);

let on1 (p : int) (q : int) (m : int list list) : int =
    let total = [
                    for x in (max 0 (p-1))..(min (p+1) (m.Length - 1)) -> 
                    [
                        for y in (max 0 (q-1))..(min (q+1) (m.[0].Length - 1)) -> 
                            if p=x && q=y then 0 else m.[x].[y]
                    ]
                ]
                |> List.collect (fun x -> x)
                |> List.fold (fun a x -> a + x) 0;
    match m.[p].[q] with
        | 0 -> if total = 3 then 1 else 0
        | 1 -> if total = 2 || total = 3 then 1 else 0;

let on2 (p : int) (q : int) (m : int list list) : int =
    match (p, q) with
        | (0, 0) -> 1
        | (0, s) when s = m.[0].Length - 1 -> 1
        | (r, 0) when r = m.Length - 1 -> 1
        | (r, s) when (r, s) = (m.Length - 1, m.[0].Length - 1) -> 1
        | (r, s) -> on1 r s m;

let rec switch (f : int -> int -> int list list -> int) (n : int) (m : int list list) : (int list list) = 
    match n with
        | 0 ->  m
        | x ->  let o = [
                            for p in 0..m.Length - 1 -> 
                            [
                                for q in 0..m.[p].Length - 1 -> f p q m
                            ]
                        ];
                switch f (x-1) o;

let run (file : string) =
    let lights = Seq.toList (File.ReadLines(file))
                    |> List.map (fun x -> parse(Seq.toList x));

    switch on1 100 lights
    |> List.fold (fun a x -> a + (List.sum x)) 0
    |> printfn "Day 18, part 1: %d";

    switch on2 100 lights
    |> List.fold (fun a x -> a + (List.sum x)) 0
    |> printfn "Day 18, part 2: %d";