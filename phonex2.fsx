let toLower x = (x:string).ToLower ()
let accentLess x = System.Text.Encoding.GetEncoding("ISO-8859-8").GetBytes(x:string) |> System.Text.Encoding.UTF8.GetString
let replace target replacement source = (source:string).Replace((target:string), replacement)
let replaceAny targets replacement source = targets |> List.fold (fun s t -> replace t replacement s) source
let remove target source = replace target "" source
let rmapping tr source = tr |> List.fold (fun s (t,r) -> replace t r s) source
let between before after replacement source target = 
    let rec between offset source = 
        if offset < (source:string).Length then 
            let lt = (target:string).Length
            let tail = source.Substring(offset)
            let idx = tail.IndexOf(target)
            if idx <> -1 && (before (tail.Substring(0, idx)) && after(tail.Substring(idx + lt))) then
                sprintf "%s%s%s" (source.Substring(0, offset + idx)) replacement (tail.Substring(idx + lt, tail.Length - idx - lt))
                |> between (idx + replacement.Length)
            else source
        else source
    between 0 source
let all = fun _ -> true
let suffix f replacement source target = between all f replacement source target
let prefix f replacement source target = between f all replacement source target

let rI = replace "y" "i"
let rF = replace "ph" "f" 
let rCh = replaceAny ["sh"; "ch"; "sch"] "5"
let rmH = remove "h"
let rGan = rmapping [ "gan", "kan"
                      "gam", "kam"
                      "gain", "kain"
                      "gaim", "kaim" ]

let [<Literal>] Vowel = "aeiou"
let isVowel x = Vowel |> Seq.exists ((=) x)
let tryIsVowel = Option.map isVowel >> Option.defaultValue false 
let tryIsNotVowel = tryIsVowel >> not

let suffixL f replacement targets source = targets |> List.fold (suffix f replacement) source

let rAin  = ["ain"; "ein"; "ain"; "eim"; "aim"] |> suffixL (Seq.tryHead >> tryIsVowel) "en"

let rO = replace "eau" "o"
let rOua = replace "oua" "2"
let rEin = replaceAny ["ein";"ain";"eim";"aim"] "4"
let rAi = replaceAny ["ai";"ei"] "e"
let rEr = replace "er" "yr"
let rEss = replace "ess" "yss"
let rEt = replace "et" "yt"
let rAn = ["an";"am";"en";"em"] |> suffixL (Seq.tryHead >> tryIsNotVowel) "1"
let rIn source = suffix (Seq.tryHead >> tryIsNotVowel) "4" source "in"
let rOn = replace "on" "1"
let rZ source = 
    let isCandidate c = ['1' .. '4'] |> List.append (Vowel |> Seq.toList) |> List.exists ((=) c)
    let tryIsCandidate = Option.map isCandidate >> Option.defaultValue false
    between (Seq.tryLast >> tryIsCandidate) (Seq.tryHead >> tryIsCandidate) "z" source "s"
let rE = replaceAny ["oe";"eu"] "e"
let rAu = replace "au" "o"
let rOi = replaceAny ["oi"; "oy"] "2"
let rOu = replace "ou" "3"
let rS = replaceAny ["ss";"sc"] "s"
let rC source = 
    let isCandidate c = "ei" |> Seq.exists ((=) c)
    let tryIsCandidate = Seq.tryHead >> Option.map isCandidate >> Option.defaultValue false
    suffix tryIsCandidate "s" source "c"
let rK = replaceAny ["c";"q";"qu";"gu"] "k"
let rGa = replace "ga" "ka"
let rGo = replace "go" "ko"
let rGy = replace "gy" "ky"

let trim letters = 
    Seq.distinct >> Seq.toArray
    >> fun y -> 
        let n = Array.length y - 1
        let c = y.[n] 
        if letters |> List.exists ((=) c) then Array.take n y 
        else y
    >> System.String

let rLast =
    rmapping [ "a","o"
               "d","t"
               "p","t"
               "j","g"
               "b","f"
               "v","f"
               "m","n" ]
    
let phonex = 
    toLower >> accentLess 
    >> rI >> rF >> rCh >> rmH 
    >> rGan >> rAin >> rO >> rOua
    >> rEin >> rAi 
    >> rEr >> rEss >> rEt
    >> rAn >> rIn >> rOn
    >> rZ >> rE >> rAu >> rOi >> rOu
    >> rS >> rC >> rK >> rGa >> rGo >> rGy
    >> rLast
    >> trim ['x';'t']

let soundex2 = 
    toLower >> accentLess
    >> fun s -> 
        if System.String.IsNullOrWhiteSpace(s) then s 
        else 
            s.Substring(1)
            |> (rmapping [ "gui", "ki"
                           "gue", "ke"
                           "ga","ka"
                           "go","ko"
                           "gu","k"
                           "ca","ka"
                           "co","ko"
                           "cu","ku"
                           "q","k"
                           "c","k"
                           "ck","k"]
                >> (Seq.map (fun c -> if isVowel c then 'a' else c) >> Seq.toArray >> System.String)
                >> replaceAny ["sh"; "sch"] "ch"
                >> rF
                >> rmH
                >> fun s -> prefix ((=) "a") "" s "y"
                >> trim ['a';'t';'d';'s']
                >> (Seq.filter ((<>)'a') >> Seq.toArray >> System.String))
            |> (Seq.distinct >> Seq.toArray >> System.String)
            |> sprintf "%c%s" s.[0]

soundex2 "yolaine"
soundex2 "yolène"
soundex2 "clement"
soundex2 "klement"

phonex "PHYLAURHEIMSMET"

phonex "clement"
phonex "clement"

phonex "yolaine"
phonex "yolène"