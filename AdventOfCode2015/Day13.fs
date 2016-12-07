module Day13

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
                    build xs ls (update p q c m);

let rec index (g : string) (n : int) (ls : (string * int) list) : (string * int) list =
    match ls with
        | [] -> [(g, n)]
        | x::xs ->  let (a, b) = x
                    if g = a then ls else x :: index g (n+1) xs;

let rec parse (s : string) : (string * string * int) =
    match s.Split(' ') with
        | [| a; "would"; "gain"; b; "happiness"; "units"; "by"; "sitting"; "next"; "to"; c |] -> (a, c.Substring(0, c.Length - 1), System.Int32.Parse(b))
        | [| a; "would"; "lose"; b; "happiness"; "units"; "by"; "sitting"; "next"; "to"; c |] -> (a, c.Substring(0, c.Length - 1), - System.Int32.Parse(b));

let rec remove (n : int) (ns : int list) : int list =
    match ns with
        | [] -> []
        | x::xs -> if x = n then xs else x :: (remove n xs);

let rec permutations (ls : int list) (t : int list) : int list list = 
    [ 
        if List.length t = List.length ls then 
            yield [] 
        else
            for l in ls do
              if not (List.exists ((=) l) t) then 
                for perm in permutations ls (t @ [l])  do
                  yield l::perm 
    ];

let rec happy (p : int) (ps : int list) (m : int list list) : int =
    match ps with
        | [] -> 0
        | [x] -> m.[x].[p] + m.[p].[x]
        | x::xs ->  let y = List.head xs;
                    m.[x].[y] + m.[y].[x] + (happy p xs m);

let run (file : string) =
    let data =  Seq.toList (File.ReadLines(file))
                    |> List.map (fun x -> parse x);
    let labels = List.fold (fun l (a, b, c) -> index b 0 (index a 0 l)) [] data;
    let graph = square labels.Length
                |> build data labels;
    permutations [0..graph.Length-1] []
    |> List.map (fun x -> happy (List.head x) x graph)
    |> List.max
    |> printfn "Day 13, part 1: %d";

    let graph2 = square (labels.Length + 1)
                 |> build data labels;
    permutations [0..graph2.Length-1] []
    |> List.map (fun x -> happy (List.head x) x graph2)
    |> List.max
    |> printfn "Day 13, part 2: %d";