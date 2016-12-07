module Day23

open System;
open System.IO;

let rec execute (p : int) (i : string list) (a : int) (b : int) : int * int =
    if p >= i.Length then
        (a, b)
    else
        match i.[p].Split(' ') with
            | [| "hlf"; x |] when x = "a" -> execute (p+1) i (a/2) b;
            | [| "hlf"; x |] when x = "b" -> execute (p+1) i a (b/2);
            | [| "tpl"; x |] when x = "a" -> execute (p+1) i (a*3) b;
            | [| "tpl"; x |] when x = "b" -> execute (p+1) i a (b*3);
            | [| "inc"; x |] when x = "a" -> execute (p+1) i (a+1) b;
            | [| "inc"; x |] when x = "b" -> execute (p+1) i a (b+1);
            | [| "jmp"; x |] -> execute (p + Int32.Parse(x)) i a b;
            | [| "jie"; x; y |] ->  if (if x = "a," then a else b) % 2 = 0 then
                                        execute (p+Int32.Parse(y)) i a b;
                                    else
                                        execute (p+1) i a b;
            | [| "jio"; x; y |] ->  if (if x = "a," then a else b) = 1 then
                                        execute (p+Int32.Parse(y)) i a b;
                                    else
                                        execute (p+1) i a b;

let run (file : string) = 
    let instr = Seq.toList (File.ReadAllLines file);

    execute 0 instr 0 0
    |> snd
    |> printfn "Day 23, part 1: %d";
    
    execute 0 instr 1 0
    |> snd
    |> printfn "Day 23, part 1: %d";
