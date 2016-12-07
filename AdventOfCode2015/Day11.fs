module Day11

open System.IO;
open System.Text;

let rec increment (is : int list) : int list =
    match is with
        | [] -> []
        | x::xs ->  if x < 26 then
                        (x+1) :: xs
                    else
                        1 :: (increment xs);

let rec increasing (a : int) (b : int) (c : int) (ds : int list) : bool =
    if (a-1) = b && (b-1) = c then 
        true
    else
        match ds with
            | [] -> false
            | x::xs -> increasing b c x xs;

let rec valid (cs : int list) : bool =
    match cs with
        | [] -> true
        | x::xs ->  if x = 0 || x = 15 || x = 12 then
                        false
                    else
                        valid xs;

let rec pairs (cs : int list) (c : int) (f : bool) : bool =
    match cs with
        | [] -> false
        | x::xs ->  if x = c then
                        if f then true else pairs (List.tail xs) (List.head xs) true
                    else 
                        pairs xs x f;

let rec check (intRep : int list) : string =
    let inc = increment intRep;
    let (x, y) = (List.head inc, List.tail inc);
    let (a, b) = (List.head y, List.tail y);
    let (p, q) = (List.head b, List.tail b);

    if (increasing x a p q) && (valid inc) && (pairs y x false) then
        List.rev inc
            |> List.map (fun (x) -> (char (x + 96)))
            |> List.fold (fun a x -> a+x.ToString()) ""
    else
        check inc;

let run (file : string) =
    let key = (Seq.toList (File.ReadAllLines(file))).[0];
    let part1 = Seq.toList key
                |> List.map (fun x -> (int x) - 96)
                |> List.rev
                |> check;
    printfn "Day 11, part 1: %s" part1;
    
    Seq.toList part1
    |> List.map (fun x -> (int x) - 96)
    |> List.rev
    |> check
    |> printfn "Day 11, part 2: %s";


