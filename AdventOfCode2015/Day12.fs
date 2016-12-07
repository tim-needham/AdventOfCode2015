module Day12

open Newtonsoft.Json;
open Newtonsoft.Json.Linq;
open System.IO;
open System.Linq;

let rec total (j : JToken) : int =
    match j.Type with
        | JTokenType.Object ->  let mutable x = 0
                                for c in (j :?> JObject).PropertyValues() do
                                    x <- x + total c;
                                x;
        | JTokenType.Array ->   let mutable x = 0
                                for i in (j :?> JArray) do
                                    x <- x + total i;
                                x;                                
        | JTokenType.Integer -> j.Value<int>()
        | _ -> 0;

let rec redlessTotal (j : JToken) : int =
    match j.Type with
        | JTokenType.Object ->  let k = j :?> JObject;
                                if k.PropertyValues().Any(fun p -> p.Type = JTokenType.String && p.Value<string>() = "red") then
                                    0
                                else
                                    let mutable x = 0
                                    for c in k.PropertyValues() do
                                        x <- x + redlessTotal c;
                                    x;
        | JTokenType.Array ->   let mutable x = 0
                                for i in (j :?> JArray) do
                                    x <- x + redlessTotal i;
                                x;                                
        | JTokenType.Integer -> j.Value<int>()
        | _ -> 0;

let run (file : string) =
    let json = File.ReadAllText(file)
                |> JsonConvert.DeserializeObject
                :?> JObject;

    json
    |> total 
    |> printfn "Day 12, part 1: %d";

    json
    |> redlessTotal 
    |> printfn "Day 12, part 2: %d";
