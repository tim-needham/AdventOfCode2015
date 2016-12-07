module Day24

open System;
open System.IO;

let parse (x : string) : int =
    Int32.Parse(x);
       
let rec pick (cs : int list) (t : int) (ps : int list) : int list list=
    match t, ps with
        | 0, _ -> [cs];
        | _, [] -> [];
        | n, x::xs ->   if x <= n then
                            (pick (x::cs) (t-x) xs) @ (pick cs t xs);
                        else
                            pick cs t xs;

let optimal (ps : int list) (c : int) : int64 =
    let t = (List.sum ps) / c;
    let ms = ps
                |> pick [] t
                |> List.filter (fun x -> List.sum x = t)
                |> List.sortBy (fun x -> List.length x);
    let m = ms
            |> List.head
            |> List.length;

    ms
    |> List.filter (fun x -> List.length x = m)
    |> List.sortBy (fun x -> List.fold (fun a y -> a * (int64 y)) 1L x)
    |> List.head
    |> List.fold (fun a y -> a * (int64 y)) 1L;


let run (file : string) =
    let parcels = File.ReadAllLines(file)
                    |> Seq.toList
                    |> List.map (fun x -> parse x)
                    |> List.sortBy (fun x -> -x);

    optimal parcels 3
    |> printfn "Day 24, part 1: %d";

    optimal parcels 4
    |> printfn "Day 24, part 2: %d";
