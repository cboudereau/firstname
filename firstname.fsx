#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#I "packages/Google.DataTable.Net.Wrapper/lib/"
#I "packages/XPlot.GoogleCharts/lib/net45/"
#r "XPlot.GoogleCharts.dll"

open FSharp.Data

type FirstNameStats = CsvProvider< "nat2016_txt/nat2016.txt", Separators = "\t", HasHeaders=true >

let stats = FirstNameStats.Load("nat2016_txt/nat2016.txt")

module Seq = 
    let batch n =
        Seq.mapi (fun i elem -> (i/n),elem) 
        >> Seq.groupBy (fun (a,_) -> a)
        >> Seq.map (fun (_,se) -> se |> Seq.map (snd) |> Seq.toArray)

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
open XPlot.GoogleCharts

type Sex = 
    | Boy = 1
    | Girl = 2

module Sex = 
    let value (s:Sex) = int s

module Phonetic = 
    let value (Phonetic p) = p
    let short = value >> fun p -> p.Length < 7
    let little = value >> fun p -> p.Length < 3
    let onlyComposed = value >> fun p -> p.Contains("-")
    let notComposed = onlyComposed >> not
    let blacklist l = 
        let bl = l |> Seq.map value |> Seq.toList
        value
        >> fun p -> bl |> List.exists ((=) p) |> not

module Snd = 
    let map f (x,y) = x, f y
module Fst = 
    let map f (x, y) = f x, y

#time
let firstNames sex = 
    stats.Rows 
    |> Seq.choose (fun x -> 
        Int32.tryInt x.Annais 
        |> Option.bind (fun y -> if x.Sexe = (sex |> Sex.value) then Some (FirstName x.Preusuel, (Year y, x.Nombre)) else None) )

let phonetic f = 
    Seq.map (fun ((FirstName n), (y, c)) -> f n |> Phonetic, (y,(FirstName n,c)))
    >> Seq.groupBy fst
    >> Seq.map ( 
        Snd.map (
            Seq.map snd 
            >> Seq.groupBy fst 
            >> Seq.map (Snd.map (Seq.map snd)) 
            >> Seq.sortBy fst))

let pGirl = firstNames Sex.Girl |> phonetic phonex

//pGirl |> Seq.map (Snd.map (Seq.collect snd >> Seq.map fst >> Seq.distinct >> Seq.toList)) |> Map.ofSeq
//|> Map.tryFind (phonex "yolène" |> Phonetic)

let boys = firstNames Sex.Boy
let pBoys = boys |> phonetic phonex

pBoys |> Seq.map (Snd.map (Seq.collect snd >> Seq.map fst >> Seq.distinct >> Seq.toList)) |> Map.ofSeq
|> Map.tryFind (phonex "clement" |> Phonetic)

boys |> Seq.map fst |> Seq.distinct |> Seq.length //14256
pBoys |> Seq.length //7634
let notComposed = pBoys |> Seq.filter (fst >> Phonetic.notComposed) 
notComposed |> Seq.length //6652
let notComposedAndShort = notComposed |> Seq.filter (fst >> Phonetic.short)
notComposedAndShort |> Seq.length //6154
let notComposedAndTooShort = notComposedAndShort |> Seq.filter (fst >> Phonetic.little >> not)
notComposedAndTooShort |> Seq.length //6021
let onlyBoys = notComposedAndTooShort |> Seq.filter (fst >> (Phonetic.blacklist (pGirl |> Seq.map fst |> Seq.toList)))
onlyBoys |> Seq.length //3923

let classic = onlyBoys |> Seq.filter (snd >> Seq.collect snd >> Seq.sumBy snd >> (<) 5000)
classic |> Seq.length //99

let result = classic

let graph average (data:(string * seq<System.DateTime*float>) list) =     
    let labels = data |> List.map fst
    let points = data |> List.map snd
    let options =
        Options(
            title = "Number of firstname by year and average",
            vAxis = Axis(title = "Count"),
            hAxis = Axis(title = "Year"),
            series =
                [|
                    yield Series(``type`` = "lines")
                    yield! labels |> Seq.map (fun _ -> Series(``type`` = "bars"))
                |]
        )
 
    (average :: points)
    |> Chart.Combo
    |> Chart.WithOptions options
    |> Chart.WithLabels ("Average" :: labels)
    |> Chart.Show

let average = 
    Seq.collect (snd >> Seq.map (Snd.map (Seq.sumBy snd)))
    >> Seq.groupBy fst
    >> Seq.map (fun (Year y,c) -> System.DateTime(y,1,1), c |> Seq.averageBy (snd >> float))
    >> Seq.sortBy fst
    >> Seq.toList

let datas = 
    Seq.map (fun (_, s) ->
        let mostUsedName = s |> Seq.collect snd |> Seq.sortByDescending snd |> Seq.map fst |> Seq.head
        mostUsedName, s |> Seq.map (Snd.map (Seq.sumBy snd)))
    >> Seq.map (fun (FirstName n, s) -> n, s |> Seq.map (fun (Year y, c) -> System.DateTime(y,1,1), float c) |> Seq.sortBy fst)

let av = average result

datas result
|> Seq.batch 5
|> Seq.iter (Seq.toList >> graph av)

datas result |> Seq.toList |> graph av