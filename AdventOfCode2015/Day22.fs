module Day22

open System;
open System.IO;

type Spell = {
    name : string;
    cost : int;
    damage : int;
    health : int;
    armour : int;
    mana : int;
    turns : int
};

let parse (x : string) : Spell =
    match x.Split(',') with
        | [| n; c; d; h; a; m; t |] ->  {
                                            name = n;
                                            cost = Int32.Parse(c);
                                            damage = Int32.Parse(d);
                                            health = Int32.Parse(h);
                                            armour = Int32.Parse(a);
                                            mana = Int32.Parse(m);
                                            turns = Int32.Parse(t)
                                        };

let rec player (best : int) (spent : int) (all : Spell list) (health : int) (mana : int) (efts : Spell list) (bHea : int) (bDam : int) (pre : int -> int) : int =
    if spent > best then 
        best;
    else
        let mana2 = mana + Seq.sumBy (fun x -> x.mana) efts;
        let bHealth = bHea - (Seq.sumBy (fun x -> x.damage) efts);
        let health2 = pre health;    

        if health2 <= 0 then
            best;
        else if bHealth <= 0 then
            spent;
        else
            let effects = efts |> List.choose (fun x -> if x.turns > 1 then Some { x with turns = x.turns - 1 } else None);
            let spells = all |> List.filter (fun x -> x.cost <= mana2 && not (List.exists (fun y -> x.name = y.name) effects));

            match spells with
                | [] -> best;
                | _ ->  spells
                        |> List.fold (fun b2 x ->   let spent2 = spent + x.cost;
                                                    let bHealth2 = bHealth - if x.turns = 1 then x.damage else 0;
                                                    if bHealth2 <= 0 then
                                                        min b2 spent2;
                                                    else
                                                        let spells = if x.turns = 1 then effects else x::effects;
                                                        let health3 = if x.turns = 1 then x.health + health2 else health2;
                                                        min b2 (boss b2 spent2 all health3 (mana2 - x.cost) spells bHealth2 bDam pre);
                                                ) best;
and boss (best : int) (spent : int) (all : Spell list) (health : int) (mana : int) (efts : Spell list) (bHea : int) (bDam : int) (pre : int -> int) : int =
    let mana2 = mana + Seq.sumBy (fun x -> x.mana) efts;
    let bHealth = bHea - (Seq.sumBy (fun x -> x.damage) efts);

    if bHealth <= 0 then
        spent;
    else
        let effects = efts |> List.choose (fun x -> if x.turns > 1 then Some { x with turns = x.turns - 1 } else None);
        let damage = max (bDam - (Seq.sumBy (fun x -> x.armour) efts)) 1;
        let health2 = health - damage;
        if health2 <= 0 then
            best
        else
            player best spent all health2 mana2 effects bHealth bDam pre;                                

let run (file : string) = 
    let lines = Seq.toList (File.ReadAllLines(file));
    let hp = lines |> Seq.head |> Int32.Parse;
    let mana = lines |> Seq.skip 1 |> Seq.head |> Int32.Parse;
    let bHealth = lines |> Seq.skip 2 |> Seq.head |> Int32.Parse;
    let bDamage = lines |> Seq.skip 3 |> Seq.head |> Int32.Parse;
    let spells = lines
                    |> Seq.skip 4
                    |> Seq.toList
                    |> List.map (fun x -> parse x);

    player Int32.MaxValue 0 spells hp mana [] bHealth bDamage (fun x -> x)
    |> printfn "Day 22, part 1: %d";

    player Int32.MaxValue 0 spells hp mana [] bHealth bDamage (fun x -> x - 1)
    |> printfn "Day 22, part 2: %d";
