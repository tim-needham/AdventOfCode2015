module Day10

open System.IO;
open System.Text;

let rec lookAndSay (cs : char list) (c : char) (n : int) (s : StringBuilder) : string =
    match cs with
        | [] -> s.Append(n).Append(c).ToString()
        | x::xs ->  if x = c then
                        lookAndSay xs c (n+1) s
                    else
                        lookAndSay xs x 1 (s.Append(n).Append(c))

let rec washRinseRepeat (s : string) (l : int) : string =
    match l with
        | 0 -> s
        | x ->  let cs = Seq.toList s;
                let sb = new StringBuilder();
                washRinseRepeat (lookAndSay (List.tail cs) (List.head cs) 1 sb) (l-1);

let run (file : string) =
    let key = (Seq.toList (File.ReadAllLines(file))).[0];
    let after40 = washRinseRepeat key 40;

    after40
    |> String.length
    |> printfn "Day 10, part 1: %d";
    
    washRinseRepeat after40 10
    |> String.length
    |> printfn "Day 10, part 2: %d";
