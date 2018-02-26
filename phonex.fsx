#I "packages/FParsec/lib/portable-net45+win8+wp8+wpa81"
#r "FParsecCS.dll"
#r "FParsec.dll"

open FParsec

let (>.) p x = p |>> fun _ -> x 
let [<Literal>] silent = "h"

//S like Sound
let pSchar c = pchar c <|> (anyOf silent >>? pchar c)
let (~~) = pSchar

let pI = ~~'y' <|> ~~'i'
run pI "y"
run pI "hy"

let pSstring s = 
    (s:string)
    |> Seq.fold (fun s x -> s >>? ~~x >. ()) (preturn ())
    >. s
run (pSstring "test") "test"
run (pSstring "test") "hthehsht"
let (!!) = pSstring

let pO = !!"au" <|> !!"eau" <|> !!"o"
run pO "eau"
run pO "ho"
run pO "hehahu"

//Score
let pCsh = 
    (!!"sc" <|> !!"s" <|> !!"c") >>? pchar 'h' >. [ 5 - 1 ] 
    <|> (pchar 'h' >. [ ])
    <??> "sound ch"

run pCsh "salut!" 
run pCsh "csalut!" 
run pCsh "chalut!" 
run pCsh "shalut!" 
run pCsh "schalut!" 

let pF = 
    ~~'p' >>? ~~'h' 
    <|> ~~'f' >. [ 6 ]
    <??> "sound f"

run pF "f"
run pF "ph"

let pNm = ~~'n' <|> ~~'m' 
run pNm "n"

let sAnyOf = Seq.fold (fun s x -> s <|> ~~x) pzero
let pVowel = sAnyOf "aeiouy"
run pVowel "a"

let pGan = !!"ga" >>? pNm >. [ 10; 1 ] <??> "sound gan"
run pGan "gan"
run pGan "gam"

let pGain = !!"ga" >>? pI >>? pNm >. [ 10; 4 - 1 ] <??> "sound gain"
run pGain "gain"
run pGain "gaim"
run pGain "hell"

let pAe = ~~'a' <|> ~~'e' 
let pAei = pAe >>? pI 

let pAin = 
    let pAeinm = pAei >>? pNm >. [ 4 - 1 ] <??> "sound ain"
    let pAinV = pAeinm >>? pVowel >. [ 20; 12 ] <??> "sound ain + vowel"
    pAinV <|> pAeinm
run pAin "ain"
run pAin "aine"

let pOua = pO >>? !!"ua" >. [ 2 - 1 ] <??> "sound oua"
run pOua "oua"

let pAi = sAnyOf "éèê" <|> pAei >. [ 20 ] <??> "sound é"
run pAi "é"

let pEr = !!"er" >. [ 20; 14 ] <??> "sound er"
run pEr "er"

let pEss = !!"ess" >. [ 20; 15 ] <??> "sound ess"
run pEss "ess"

let pEt = !!"et" >. [ 20; 16 ] <??> "sound et"
run pEt "et"

let pAn = pAe >>? pNm >. [ 1 - 1 ] <??> "sound an"
run pAn "an"

let pIn = pI >>? ~~'n' >. [ 4 - 1 ] <??> "sound in"
run pIn "in"

let pOn = pO >>? ~~'n' >. [ 1 - 1 ] <??> "sound on"
run pOn "on"

let pOe = (pO >>? !!"e") <|> !!"eu" >. [ 5 ] <??> "sound oe"
run pOe "oe"

let poGo = 
    (~~'g' >>? pO >. [ 10; 13 ]) 
    <|> (pO >. [ 13 ])
    <??> "sound o or go"
run poGo "go"
run poGo "o"
run poGo "go"
run poGo "gau"

let pOi = pO >>? pI >. [ 2 - 1 ] <??> "sound oi"
run pOi "oi"
run pOi "oy"

let pOu = !!"ou" >. [ 2 - 1 ] <??> "sound ou"
run pOu "ou"

let pSs = !!"ss" <|> !!"sc" >. [ 15 ] <??> "sound s"
run pSs "ss"
run pSs "sc"

let pCe = !!"ce" >. [ 15; 5 ] <??> "sound ce"
run pCe "ce"
let pCi = ~~'c' >>? pI >. [ 15; 9 ] <??> "sound ci"
run pCi "ci"

let pK = pSstring "c" <|> pSstring "q" <|> pSstring "qu" <|> pSstring "gu" >. [ 10 ] <??> "sound k"
run pK "c"

let pGa = pSstring "ga" <|> pSstring "go" >. [ 10; 13 ] <??> "sound ga"
run pGa "ga"

let pGy = pSstring "gy" >. [ 10; 20 ] <??> "sound gy"

let pMapping = 
    let letters = "adpjbvm" |> Seq.toList
    let scores = [ 13; 16; 16; 7; 6; 6; 12 ]
    List.zip letters scores
    |> List.fold (fun s (c, score) -> s <|> (~~c >. score)) pzero
    |>> List.singleton
    <??> "sound in the mapping"
run pMapping "a"

let pRest = 
    let letters = "efghiklnorstuwxyz" |> Seq.toList
    let scores = [ 5 .. 21 ]
    List.zip letters scores
    |> List.fold (fun s (c, score) -> s <|> (~~c >. score)) pzero
    |>> List.singleton
    <??> "letter in the encoding phase"
run pRest "e"

let p1 = 
        pCsh 
    <|> pF 
    <|> pGan 
    <|> pGain 
    <|> pAin
    <|> pOua
    <|> pAi
    <|> pEr
    <|> pEss
    <|> pEt
    <|> pAn
    <|> pIn
    <|> pOn
    <|> pOe
    <|> poGo
    <|> pOi
    <|> pOu
    <|> pSs
    <|> pCe
    <|> pCi
    <|> pK
    <|> pGa
    <|> pGy
    
    <|> pMapping
    <|> pRest

let p = many p1 |>> List.concat

run p "salut!" 
run p "csalut!" 
run p "csalut!" 
run p "chalut!" 
run p "shalut!" 
run p "schalut!" 
run p "gan"
run p "gam"
run p "gain"
run p "gaim"


run p "clément"