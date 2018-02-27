#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#I "packages/Google.DataTable.Net.Wrapper/lib/"
#I "packages/XPlot.GoogleCharts/lib/net45/"
#r "XPlot.GoogleCharts.dll"

open FSharp.Data

type FirstNameStats = CsvProvider< "nat2016_txt/nat2016.txt", Separators = "\t", HasHeaders=true >

let stats = FirstNameStats.Load("nat2016_txt/nat2016.txt")

module Int32 = 
    let tryInt x = 
        match System.Int32.TryParse(x) with
        | true, i -> Some i
        | _ -> None

type [<Struct>] Phonetic = Phonetic of string

module Text = 
    open System
    let private str x= (x:string) :> char seq

    let soundex x =
        match str x |> Seq.map Char.ToUpper |> Seq.toList with
        | [] -> []
        | h :: t -> 
            List.Cons(h,
                t
                |> List.choose(function
                    | 'B' | 'F' | 'P' | 'V' -> Some '1'
                    | 'C' | 'G' | 'J' | 'K' | 'Q' | 'S' | 'X' | 'Z' -> Some '2' 
                    | 'D' | 'T' -> Some '3'
                    | 'L' -> Some '4'
                    | 'M' | 'N' -> Some '5'
                    | 'R' -> Some '6'
                    | _ -> None) )
        |> List.distinct
        |> List.toArray
        |> String
        |> fun x -> x.PadRight(6,'0')
        |> fun x -> x.Substring(0, 6)
        |> Phonetic

open Text

type [<Struct>] Year = Year of int
type [<Struct>] FirstName = FirstName of string

#load "phonex2.fsx"        

open Phonex2

phonex "clement"

#time
let boys = 
    stats.Rows 
    |> Seq.choose (fun x -> 
        Int32.tryInt x.Annais 
        |> Option.bind (fun y -> if x.Sexe = 1 then Some (phonex x.Preusuel, (Year y, (FirstName x.Preusuel, x.Nombre))) else None) )
    |> Seq.toList

let datas = 
    boys
    |> Seq.groupBy fst
    |> Seq.map (fun (code, d) -> 
        code, d |> Seq.map snd |> Seq.groupBy fst |> Seq.toList )
    |> Map.ofSeq

datas |> Map.tryFind (phonex "clément")

phonex "clément"