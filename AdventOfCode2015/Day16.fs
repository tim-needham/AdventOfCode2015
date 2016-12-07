module Day16

open System;
open System.IO;
open System.Text.RegularExpressions;

type Aunt = { n : int; ch : int; ca : int; s : int; po : int; a : int; v : int; g : int; t : int; cr : int; pe : int; };;

let aunt (n : int) (ch : int) (ca : int) (s : int) (po : int) (a : int) (v : int) (g : int) (t : int) (cr : int) (pe : int) : Aunt =
    { n = n; ch = ch; ca = ca; s = s; po = po; a = a; v = v; g = g; t = t; cr = cr; pe = pe; };

let rec grab (ls : string list) (a : Aunt) : Aunt =
    match ls with
        | [] -> a
        | x::xs ->  let b = grab xs a;
                    match x.Split(':') with
                        | [| " children"; y |] -> aunt (b.n) (Int32.Parse(y)) (b.ca) (b.s) (b.po) (b.a) (b.v) (b.g) (b.t) (b.cr) (b.pe)
                        | [| " cats"; y |] -> aunt (b.n) (b.ch) (Int32.Parse(y)) (b.s) (b.po) (b.a) (b.v) (b.g) (b.t) (b.cr) (b.pe)
                        | [| " samoyeds"; y |] -> aunt (b.n) (b.ch) (b.ca) (Int32.Parse(y)) (b.po) (b.a) (b.v) (b.g) (b.t) (b.cr) (b.pe)
                        | [| " pomeranians"; y |] -> aunt (b.n) (b.ch) (b.ca) (b.s) (Int32.Parse(y)) (b.a) (b.v) (b.g) (b.t) (b.cr) (b.pe)
                        | [| " akitas"; y |] -> aunt (b.n) (b.ch) (b.ca) (b.s) (b.po) (Int32.Parse(y)) (b.v) (b.g) (b.t) (b.cr) (b.pe)
                        | [| " vizslas"; y |] -> aunt (b.n) (b.ch) (b.ca) (b.s) (b.po) (b.a) (Int32.Parse(y)) (b.g) (b.t) (b.cr) (b.pe)
                        | [| " goldfish"; y |] -> aunt (b.n) (b.ch) (b.ca) (b.s) (b.po) (b.a) (b.v) (Int32.Parse(y)) (b.t) (b.cr) (b.pe)
                        | [| " trees"; y |] -> aunt (b.n) (b.ch) (b.ca) (b.s) (b.po) (b.a) (b.v) (b.g) (Int32.Parse(y)) (b.cr) (b.pe)
                        | [| " cars"; y |] -> aunt (b.n) (b.ch) (b.ca) (b.s) (b.po) (b.a) (b.v) (b.g) (b.t) (Int32.Parse(y)) (b.pe)
                        | [| " perfumes"; y |] -> aunt (b.n) (b.ch) (b.ca) (b.s) (b.po) (b.a) (b.v) (b.g) (b.t) (b.cr) (Int32.Parse(y));

let parse (s : string) : Aunt =
    let m = Regex.Match(s, "Sue (\\d*)\:(.*)");
    let a = aunt (Int32.Parse(m.Groups.[1].Value)) -1 -1 -1 -1 -1 -1 -1 -1 -1 -1;
    let fs = Seq.toList (m.Groups.[2].Value.Split(','));
    grab fs a;

let matches (a : Aunt) (r : Aunt) : bool =
    (a.ch = -1 || a.ch = r.ch) &&
    (a.ca = -1 || a.ca = r.ca) &&
    (a.s = -1 || a.s = r.s) &&
    (a.po = -1 || a.po = r.po) &&
    (a.a = -1 || a.a = r.a) &&
    (a.v = -1 || a.v = r.v) &&
    (a.g = -1 || a.g = r.g) &&
    (a.t = -1 || a.t = r.t) &&
    (a.cr = -1 || a.cr = r.cr) &&
    (a.pe = -1 || a.pe = r.pe);

let retroencabulate (a : Aunt) (r : Aunt) : bool =
    (a.ch = -1 || a.ch = r.ch) &&
    (a.ca = -1 || a.ca > r.ca) &&
    (a.s = -1 || a.s = r.s) &&
    (a.po = -1 || a.po < r.po) &&
    (a.a = -1 || a.a = r.a) &&
    (a.v = -1 || a.v = r.v) &&
    (a.g = -1 || a.g < r.g) &&
    (a.t = -1 || a.t > r.t) &&
    (a.cr = -1 || a.cr = r.cr) &&
    (a.pe = -1 || a.pe = r.pe);

let rec findAunts (ls : Aunt list) (r : Aunt) (f : Aunt -> Aunt -> bool) : Aunt list =
    match ls with
        | [] -> []
        | x::xs ->  let ys = findAunts xs r f;
                    if f x r then x::ys else ys;

let run (file : string) =
    let data =  Seq.toList (File.ReadLines(file))
                    |> List.map (fun x -> parse x);
    let req = aunt -1 3 7 2 3 0 0 5 3 2 1;

    findAunts data req matches
    |> List.head
    |> (fun x -> x.n)
    |> printfn "Day 16, part 1: %d";

    findAunts data req retroencabulate
    |> List.head
    |> (fun x -> x.n)
    |> printfn "Day 16, part 2: %d";
