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
