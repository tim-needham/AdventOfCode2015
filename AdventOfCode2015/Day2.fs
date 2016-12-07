module Day2

open System;
open System.IO;

let parse (d : string) : (int * int * int) =
    let ds = d.Split('x');
    (Int32.Parse(ds.[0]), Int32.Parse(ds.[1]), Int32.Parse(ds.[2]));

let sides (l : int) (w : int) (h : int) : int list =
    [(l * w); (l * h); (w * h)];

let ribbon (ss : int list) : int =
    Seq.sort ss
    |> Seq.take 2
    |> Seq.toList
    |> List.sum;
 
let wrap (d : int * int * int) : int =
    let (l, w, h) = d;
    let s = sides l w h;
    (List.min s) + (List.fold (fun a x -> a + (2 * x)) 0 s);

let tie (d : int * int * int) : int =
    let (l, w, h) = d;
    (2 * ribbon (l :: (w :: [h]))) + (l * w * h);

let run (file : string) =
    let dimensions = Seq.toList (File.ReadLines(file))
                        |> List.map (fun x -> parse x);

    List.fold (fun a d -> a + (wrap d)) 0 dimensions
    |> printfn "Day 2, part 1: %d";

    List.fold (fun a d -> a + (tie d)) 0 dimensions
    |> printfn "Day 2, part 2: %d";
