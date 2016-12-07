module Day21

open System;
open System.IO;

let parse (x : string) : string * string * int * int *int  =
    match x.Split(' ') with
        | [| "Weapon"; a; b; c; d |] -> ("W", a, Int32.Parse(b), Int32.Parse(c), Int32.Parse(d));
        | [| "Armor"; a; b; c; d |] -> ("A", a, Int32.Parse(b), Int32.Parse(c), Int32.Parse(d));
        | [| "Ring"; a; b; c; d |] -> ("R", a, Int32.Parse(b), Int32.Parse(c), Int32.Parse(d));

let rec combinations (a : 'a list) (z : int) (s : 'a list) : seq<'a list> = seq {
    match z, s with 
        | n, x::xs -> 
            if n > 0 then yield! combinations (x::a) (n - 1) xs
            if n >= 0 then yield! combinations a n xs 
        | 0, [] -> yield a 
        | _, [] -> () 
}

let loadout (i : (string * string * int * int * int) list) : seq<(string * string * int * int * int) list> =
    let weapons = List.filter (fun x -> match x with
                                            | ("W", _, _, _, _) -> true
                                            | _ -> false) i;
    let armour = ("A", "None", 0, 0, 0) ::
                    List.filter (fun x -> match x with
                                            | ("A", _, _, _, _) -> true
                                            | _ -> false) i;
    let rings = List.filter (fun x -> match x with
                                            | ("R", _, _, _, _) -> true
                                            | _ -> false) i;

    let rc = (combinations [] 1 rings, combinations [] 2 rings)
                ||> (fun x y -> Seq.append x y )
                |> Seq.append [[]];

    seq { 
        for w in weapons -> seq { 
            for a in armour -> seq { 
                for r in rc -> [w; a] @ r 
            }
        } 
    }
    |> Seq.collect (fun x -> x)
    |> Seq.collect (fun x -> x);

let stats (h : int) (i : (string * string * int * int * int) list) : int * int * int * int =
    List.fold (fun (a, b, c) (t, s, x, y, z) -> (a + x, b + y, c + z)) (0, 0, 0) i
    |> fun (x, y, z) -> (h, x, y, z);

let rec wins (l : int * int * int * int) (b : int * int * int) : bool =
    match l, b with
        | _, (p, _, _) when p <= 0 -> true
        | (p, _, _, _), _ when p <= 0 -> false
        | (a, b, c, d), (x, y, z) ->    let bd = x - (max 1 (c - z));
                                        let pd = a - (max 1 (y - d));
                                        wins (pd, b, c, d) (bd, y, z);

let run (file : string) = 
    let lines = Seq.toList (File.ReadAllLines(file));
    let hp = lines |> Seq.head |> Int32.Parse;
    let b1 = lines |> Seq.skip 1 |> Seq.head |> Int32.Parse;
    let b2 = lines |> Seq.skip 2 |> Seq.head |> Int32.Parse;
    let b3 = lines |> Seq.skip 3 |> Seq.head |> Int32.Parse;
    let boss = (b1, b2, b3);
    let data = lines
                |> Seq.skip 4
                |> Seq.toList
                |> List.map (fun x -> parse x);

    loadout data
    |> Seq.map (fun x -> stats hp x)
    |> Seq.filter (fun x -> wins x boss)
    |> Seq.map (fun (a, b, c, d) -> b)
    |> Seq.sort
    |> Seq.head
    |> printfn "Day 21, part 1: %d";

    loadout data
    |> Seq.map (fun x -> stats hp x)
    |> Seq.filter (fun x -> not (wins x boss))
    |> Seq.map (fun (a, b, c, d) -> b)
    |> Seq.sortBy(fun x -> -x)
    |> Seq.head
    |> printfn "Day 21, part 2: %d";
