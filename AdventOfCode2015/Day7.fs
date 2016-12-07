module Day7

open System.IO;
open System.Text.RegularExpressions;

let (|Int|_|) s =
    match System.Int32.TryParse(s) with
        | (true, int) -> Some(int)
        | _ -> None;

let (|RegexGroup|_|) p i =
    let m = Regex.Match(i, p)
    if (m.Success) then Some m.Groups else None;

let rec parseSignals (ss : string list) (sd : (string * string) list) : (string * string) list = 
    match ss with
        | [] -> sd
        | x :: xs -> let m = Regex.Match(x, @"(.*) -> (.*)")
                     (m.Groups.[2].Value, m.Groups.[1].Value) :: parseSignals xs sd;

let rec indexOf (vs : (string * int) list) (k : string) (n : int) : int =
    match vs with
        | [] -> -1
        | x::xs ->  let (a, b) = x;
                    if a = k then n else indexOf xs k (n+1);

let rec eval (k : string) (s : (string * string) list) (vs : (string * int) list) : (int * ((string * int) list)) =
    match k with
        | Int x -> (x, vs)
        | _ ->  let i = indexOf vs k 0;
                if i >= 0 then
                    let (p, q) = vs.Item(i);
                    (q, vs);
                else
                    match List.tryFind(fun (m, _) -> m = k) s with
                        | Some (c, d) ->    let (u, v) = match d with
                                                            | Int e -> (e, vs)
                                                            | RegexGroup "NOT (.*)" e ->    let (j, k) = (eval e.[1].Value s vs) 
                                                                                            (~~~j, k)
                                                            | RegexGroup "(.*) AND (.*)" e ->   let (j, k) = (eval e.[1].Value s vs)
                                                                                                let (l, m) = (eval e.[2].Value s k)
                                                                                                (j &&& l, m)
                                                            | RegexGroup "(.*) OR (.*)" e ->    let (j, k) = (eval e.[1].Value s vs)
                                                                                                let (l, m) = (eval e.[2].Value s k)
                                                                                                (j ||| l, m)
                                                            | RegexGroup "(.*) LSHIFT (.*)" e ->    let (j, k) = (eval e.[1].Value s vs)
                                                                                                    let (l, m) = (eval e.[2].Value s k)
                                                                                                    (j <<< l, m)
                                                            | RegexGroup "(.*) RSHIFT (.*)" e ->    let (j, k) = (eval e.[1].Value s vs)
                                                                                                    let (l, m) = (eval e.[2].Value s k)
                                                                                                    (j >>> l, m)
                                                            | e -> eval e s vs
                                            (u, (k, u) :: v)                                            
                        | None -> (0, vs);

let rec repl (t : string) (v : string) (ss : (string * string) list) : (string * string) list =
    match ss with
        | [] -> []
        | x::xs ->  let (a, b) = x;
                    if a = t then
                        (a, v) :: xs
                    else
                        x :: (repl t v xs);

let run (file : string) = 
    let commands = List.rev (Seq.toList (File.ReadLines(file)));
    let signals = parseSignals commands [];
    let (result, values) = eval "a" signals [];
    printfn "Day 7, part 1: %d" result;

    let signals2 = repl "b" (result.ToString()) signals;
    let (result2, values2) = eval "a" signals2 [];
    printfn "Day 7, part 2: %d" result2;



