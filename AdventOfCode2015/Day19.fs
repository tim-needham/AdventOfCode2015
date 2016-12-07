module Day19

open System.IO;
open System.Text.RegularExpressions;

let parse (s : string) : string * string =
    match s.Split(' ') with
        | [| a; "=>"; b |] -> (a, b);

let replace (m : string) (i : string) (o : string) : seq<string> = 
    Regex.Matches(m, i)
    |> Seq.cast<Match>
    |> Seq.map (fun x -> 
        sprintf "%s%s%s" (m.Substring(0, x.Index)) o (m.Substring(x.Index+x.Length)) )
        
let distinct (p : (string * string) list) (m : string) : seq<string> =
    p
    |> Seq.map (fun (x, y) -> replace m x y)
    |> Seq.collect (fun x -> x)
    |> Seq.distinct

let rec backstep (r : (string * string) list) (t : string) (s : string) (n : int) : int =
    match s with
        | x when x = t -> n;
        | _ ->  let (p, q) = r
                            |> List.fold (fun (a, b) (c, d) ->  let e = replace a c d;
                                                                if Seq.isEmpty e then
                                                                    (a, b)
                                                                else
                                                                    (Seq.head e, b+1)) (s, n);
                backstep r t p q;

let run (file : string) =
    let data = Seq.toList (File.ReadLines(file));
    let prods = data 
                |> List.filter (fun x -> x.IndexOf("=>") >= 0)
                |> List.map (fun x -> parse x);
    let mol = data
                |> List.rev
                |> List.head;
    let revProds = prods
                    |> Seq.map (fun (x, y) -> (y, x))
                    |> Seq.sortBy (fun (x, _) -> -x.Length)
                    |> Seq.toList;
                        
    distinct prods mol
    |> Seq.length
    |> printfn "Day 19, part 1: %d";

    backstep revProds "e" mol 0
    |> printfn "Day 19, part 2: %d";
