module Day4

open System.IO;
open System.Security.Cryptography;
open System.Text;

let md5 (data : byte array) : byte array =
    use md5 = MD5.Create()
    md5.ComputeHash(data);

let rec find5 (n : int) (k : string) : int =
    let b = md5 (Encoding.ASCII.GetBytes(k + n.ToString()));
    match b.[0] with
        | 0uy ->    match b.[1] with
                        | 0uy ->    match b.[2] with
                                        | x when x < 16uy -> n
                                        | _ -> find5 (n+1) k;
                        | _ -> find5 (n+1) k;
        | _ -> find5 (n+1) k;

let rec find6 (n : int) (k : string) : int =
    let b = md5 (Encoding.ASCII.GetBytes(k + n.ToString()));
    match b.[0] with
        | 0uy ->    match b.[1] with
                        | 0uy ->    match b.[2] with
                                        | 0uy -> n
                                        | _ -> find6 (n+1) k;
                        | _ -> find6 (n+1) k;
        | _ -> find6 (n+1) k;

let run (file : string) =
    let key = (Seq.toList (File.ReadAllLines(file))).[0];

    let part1 = find5 1 key;
    printfn "Day 4, part 1: %d" part1;

    find6 (part1) key
    |> printfn "Day 4, part 2: %d";
