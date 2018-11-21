open System
open System.Collections.Generic
open System.Security.Cryptography
let s = """
                    "P-", "M-", "N-", "-N", "-M", "-P",
                    "C-", "T-", "F-", "L-", "-L", "-F", "-T", "-H",
                    "S-", "H-", "R-", "Y-", "O-", "I", "-A", "-C", "-R", "-+", "-S",
                    "+1-", "+2-", "E-", "I", "-U", "-^1", "-^2"
"""

let sp x sep = s.Split(sep) |> Array.map (fun x -> x.Trim())
let y = sp s [|','|] 
y |> Array.length
y |> Array.iter (fun x -> printfn ("%s") x)

let m = """
           P- M- N-         -N -M -P
        C- T- F- L-         -L -F -T -H
        S- H- R- Y- O- I -A -C -R -+ -S
          +1-  +2-  E- I -U  -^1  -^2
"""

let n = sp m [|' '|] |> Array.distinct // this drops the 2nd I and keeps a single " "
n |> Array.length
n |> Array.iter (fun x -> printfn ("%s") x)

let PalanDic = @"C:\Users\jackc\Git\plover_palantype\plover_palantype\dictionaries\palan_sample.json"

let readLines filePath = System.IO.File.ReadLines(filePath)
let p = readLines PalanDic

let q = """
        'S-': 'S-',
        'C-': 'C-',
        'P-': 'P-',
        'T-': 'T-',
        'H-': 'H-',
        '+-': ('+1-', '+2-'),
        'M-': 'M-',
        'F-': 'F-',
        'R-': 'R-',
        'N-': 'N-',
        'L-': 'L-',
        'Y-': 'Y-',
        'O-': 'O-',
        'E-': 'E-',
        '-A': '-A',
        '-U': '-U',
        'I': 'I',
        '-^': ('-^1', '-^2'),
        '-N': '-N',
        '-L': '-L',
        '-C': '-C',
        '-M': '-M',
        '-F': '-F',
        '-R': '-R',
        '-P': '-P',
        '-T': '-T',
        '-+': '-+',
        '-S': '-S',
        '-H': '-H',
"""

let r = q.Split('\n');;

let getFirst (x:string)  = 
    let a = Array.head (x.Split(':'))
    let b = a.Trim()
    let c = b.Trim('\'')
    c.Trim('-')

let out = Array.map (fun x -> getFirst x) r |> Array.distinct |> Array.fold (fun acc item -> acc + item) "-"

module beepUtils = 
    let laptopDir = @"D:\downloads\beep.tar\beep\"
    let desktopDir =  @"S:\DadOnly\Downloads\beep.tar\beep\"
    let beepDirectory = laptopDir
    let beepFile = beepDirectory + @"beep-1.0"

    // Get rid of comment lines
    let beep = System.IO.File.ReadLines(beepFile) 
                        |> Seq.skipWhile 
                            (fun (x:string) ->  let y = x.Trim()
                                                y.StartsWith "#")

    type Beep = { Word: string; Phonemes:  string list}

    let splitItUp (x:string) = 
      let xs = x.Split() 
                    |> Seq.choose (fun x ->
                        match x with
                          | "" -> None
                          | _ -> Some(x) )
      {Word = (Seq.head xs); Phonemes = (Seq.tail xs |> Seq.toList)}
    let data =  beep |> Seq.map splitItUp

    let phonecode =  System.IO.File.ReadLines(beepDirectory + @"phoncode.doc") 

    let arpabet = Seq.tail phonecode 
                    |> Seq.map (fun (x:string) -> x.Split('\t') |> Array.head) 
                    |> Seq.toList 
                    |> List.filter (fun x -> x.Length < 3 && x.Length > 0 )

    let vowels, consonanats = List.partition 
                                (fun (x:string) -> match x.[0] with  
                                                     | 'a' | 'e' | 'i' | 'o' | 'u' -> true 
                                                     | _ -> false) arpabet


module ConsonantProximity = 
// https://stackoverflow.com/questions/53381162/promixity-in-collection Q & A
    open beepUtils

    let rec chopIt splitter lst = 
        let i = List.tryFindIndex splitter lst
        match i with 
        | Some idx ->   
                let a, b = List.splitAt idx lst
                match List.tail b with
                | [] -> [a]
                | more -> a :: chopIt splitter more
        | None -> 
                [lst]

    let isVowel = (ResizeArray beepUtils.vowels).Contains

    let consonantClusters =
        seq {for i in beepUtils.data do yield! (chopIt isVowel i.Phonemes ) } 
        |> Seq.filter (fun x -> x <> [])
        |> Seq.toList

    let consonantPositions = Seq.collect (fun x ->  
        x |> Seq.mapi(fun i c-> (c,i + 1)::(c, -List.length x + i )::[])) 

    let results x = consonantPositions x 
                    |> Seq.collect (fun x -> x)
                    |> Seq.groupBy fst
                    |> Seq.map (fun (x, xs) -> x, Seq.countBy snd xs)

    let consonantClusterResults = results consonantClusters

    let wordResults = results (seq {for i in beepUtils.data do yield i.Phonemes } |> Seq.toList) 
                        |> Seq.filter (fun (i,_) -> not (isVowel i))

    let sorted x = x
                    |> Seq.sortBy (fun (i,_) -> i)
                    |> Seq.map (fun (x, xs) -> x, Seq.sortByDescending snd xs)
                    |> Seq.sortByDescending  (fun (_,xs) -> Seq.head xs |> (fun (_,b) -> b) )

    let print (x) = sorted x |> Seq.iter (printfn "%A")

module answer2 =
    open ConsonantProximity
    let genTail2 xss =
        let rec aux accl accr = function
        | [] -> List.rev accr
        | x::xs -> aux (x::accl) ((x, (xs, accl))::accr) xs
        aux [] [] xss

    let dist2 pred (x, (ls, rs)) =
        let rec aux n i = function
        | [] -> None
        | y::ys -> if pred y then Some(x, n) else aux (n + i) i ys
        aux -1 -1 ls, aux 1 1 rs

    let tuple2List = function
    | None, None -> []
    | Some x, None | None, Some x -> [x]
    | Some x, Some y -> [y; x]
(* 
    let isVowel = ResizeArray['a';'e';'i';'o';'u'].Contains

    "consonants"
    |> fun s -> s.ToLower()
    |> Seq.toList
    |> genTail2
    |> List.filter (fst >> isVowel >> not)
    |> List.collect (dist2 isVowel >> tuple2List)
    |> Seq.groupBy fst
    |> Seq.map (fun (x, xs) -> x, Seq.countBy snd xs)
    |> Seq.iter (printfn "%A")
 *)
    
    let testing =
                    (seq {for i in (Seq.take 10 beepUtils.data) do yield i.Phonemes } |> Seq.toList)     
                    |> genTail2
                    |> Seq.filter (fun (i,_) -> not (isVowel (List.head i)))
                    |> Seq.collect (dist2 ConsonantProximity.isVowel >> tuple2List)

                    

