module Day15

open System;
open System.IO;

type Ingredient = {
    name : string;
    capacity : int;
    durability : int;
    flavour : int;
    texture : int;
    calories : int
};

let parse (s : string) : Ingredient =
    match s.Split([| ' '; ','; ':' |], StringSplitOptions.RemoveEmptyEntries) with
        | [| a; "capacity"; b; "durability"; c; "flavor"; d; "texture"; e; "calories"; f |] ->  
            { 
                name = a; 
                capacity = Int32.Parse(b); 
                durability = Int32.Parse(c); 
                flavour = Int32.Parse(d); 
                texture = Int32.Parse(e); 
                calories = Int32.Parse(f) 
            };

let rec distribute (s : int list) (t : int) (m : int) : int list list = 
    [
        let r = t - (List.sum s)

        match List.length s with
            | l when l = m - 1 -> yield r::s
            | _ -> for n in 0..r do yield! distribute (n::s) t m
    ];

let score (is : Ingredient list) (m : (int list)) : int * int =
    let s = is
            |> List.zip m
            |> List.map (fun (a, i) -> Array.map ((*) a) [| i.capacity; i.durability; i.flavour; i.texture; i.calories |])
            |> List.reduce (Array.map2 (+));
    ((max 0 s.[0]) * (max 0 s.[1]) * (max 0 s.[2]) * (max 0 s.[3]), s.[4]);

let run (file : string) =
    let data =  Seq.toList (File.ReadLines(file))
                    |> List.map (fun x -> parse x);

    let cookies = distribute [] 100 data.Length
                    |> List.map (fun x -> score data x);

    cookies
    |> List.map fst
    |> List.max
    |> printfn "Day 15, part 1: %d";

    cookies
    |> List.maxBy (fun (s, c) -> match c with
                                    | 500 -> s
                                    | _ -> 0)
    |> fst
    |> printfn "Day 15, part 2: %d";
    