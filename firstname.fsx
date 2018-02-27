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

type [<Struct>] Year = Year of int
type [<Struct>] FirstName = FirstName of string

#load "phonex2.fsx"        

open Phonex2

#time
let boys = 
    stats.Rows 
    |> Seq.choose (fun x -> 
        Int32.tryInt x.Annais 
        |> Option.bind (fun y -> if x.Sexe = 1 then Some (phonex x.Preusuel |> Phonetic, (Year y, (FirstName x.Preusuel, x.Nombre))) else None) )
    |> Seq.toList

let datas = 
    boys
    |> Seq.groupBy fst
    |> Seq.map (fun (code, d) -> 
        code, d |> Seq.map snd |> Seq.groupBy fst |> Seq.map (fun (k,v) -> k, v |> Seq.map snd) |> Seq.sortBy fst |> Seq.toList )
    |> Map.ofSeq

datas |> Map.tryFind (phonex "clément" |> Phonetic)

phonex "clément"