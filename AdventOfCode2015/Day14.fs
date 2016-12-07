module Day14

open System;
open System.IO;

let parse (s : string) : (string * int * int * int) =
    match s.Split(' ') with
        | [| a; "can"; "fly"; b; "km/s"; "for"; c; "seconds,"; "but"; "then"; "must"; "rest"; "for"; d; "seconds." |] -> (a, Int32.Parse(b), Int32.Parse(c), Int32.Parse(d));

let rec distance (r : (string * int * int * int)) (t : int) : (string * int) =
    let (a, b, c, d) = r;
    match t with
        | 0 -> (a, 0);
        | x ->  if x > (c+d) then
                    let (p, q) = distance r (t-c-d);
                    (p, (b * c) + q);
                else if x > c then
                    (a, b * c)
                else
                    (a, b * t);

let rec fastest (rs : (string * int) list) : (string * int) =
    match rs with
        | [x] -> x
        | x::xs ->  let (a, b) = x;
                    let (p, q) = fastest xs;
                    if b>q then x else (p, q);

// name, speed, run limit, rest limit, run time, rest time, distance, score
let move (r : (string * int * int * int * int * int * int * int)) : string * int * int * int * int * int * int * int =
    let (n, s, rl, tl, r, t, d, sc) = r;
    if r > 0 then
        (n, s, rl, tl, r-1, t, d + s, sc);
    else if t > 0 then
        (n, s, rl, tl, r, t-1, d, sc);
    else 
        (n, s, rl, tl, rl-1, tl, d + s, sc);

let rec leaders (rs : (string * int * int * int * int * int * int *int) list) : (string list * int) =
    match rs with
        | [x] ->    let (a, _, _, _, _, _, d, _) = x;
                    ([a], d);
        | x::xs ->  let (a, _, _, _, _, _, d, _) = x;
                    let (bs, e) = leaders xs;
                    if d > e then 
                        ([a], d) 
                    else if d = e then
                        (a :: bs, e)
                    else (bs, e);

let rec score (s : string) (rs : (string * int * int * int * int * int * int *int) list) : (string * int * int * int * int * int * int *int) list =
    match rs with
        | [] -> rs
        | x::xs ->  let (a, b, c, d, e, f, g, h) = x;
                    (if (a = s) then (a, b, c, d, e, f, g, h+1) else x) :: (score s xs);

let rec race (rs : (string * int * int * int * int * int * int *int) list) (t : int) : (string * int * int * int * int * int * int *int) list =
    if t = 0 then
        rs
    else
        let ss = List.map (fun x -> move x) rs;
        let (ps, _) = leaders ss;
        let ts = List.fold (fun ts x -> score x ts) ss ps;
        race ts (t-1);

let rec winner (rs : (string * int * int * int * int * int * int *int) list) : (string * int) =
    match rs with
        | [x] ->    let (a, _, _, _, _, _, _, h) = x
                    (a, h);
        | x::xs ->  let (a, _, _, _, _, _, _, h) = x;
                    let (b, i) = winner xs;
                    if h > i then (a, h) else (b, i);

let run (file : string) =
    let data =  Seq.toList (File.ReadLines(file))
                    |> List.map (fun x -> parse x);
    let race1 = List.map (fun x -> distance x 2503) data
    let (r, d) = fastest race1;
    printfn "Day 14, part 1: %d" d;

    let race2 = race (List.map (fun x ->    let (a, b, c, d) = x;
                                            (a, b, c, d, c, d, 0, 0)) data) 2503;
    let (n, s) = winner race2;
    printfn "Day 14, part 2: %d" s;