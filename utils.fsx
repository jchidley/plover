open System
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
    let beepFile = @"S:\DadOnly\Downloads\beep.tar\beep\beep-1.0"
    // @"S:\DadOnly\Downloads\beep.tar\beep\beep-1.0"
    // @"D:\downloads\beep.tar\beep\beep-1.0"

    let trimmedBeep = System.IO.File.ReadLines(beepFile) 
                        |> Seq.skipWhile 
                            (fun (x:string) ->  let y = x.Trim()
                                                y.StartsWith "#")

    type Beep = { Word: string; Phonemes:  string list}
    let beepSorter (x:Beep) = - List.length x.Phonemes

    let splitItUp (x:string) = 
      let xs = x.Split() 
                    |> Seq.choose (fun x ->
                        match x with
                          | "" -> None
                          | _ -> Some(x) )
      {Word = (Seq.head xs); Phonemes = (Seq.tail xs |> Seq.toList)}

    let take x = Seq.take x trimmedBeep |> Seq.map splitItUp |> Seq.sortBy beepSorter
    
    let phonecode =  System.IO.File.ReadLines(@"S:\DadOnly\Downloads\beep.tar\beep\phoncode.doc") 

    let arpabet = Seq.tail phonecode 
                    |> Seq.map (fun (x:string) -> x.Split('\t') |> Array.head) 
                    |> Seq.toList 
                    |> List.filter (fun x -> x.Length < 3 && x.Length > 0 )

    let vowels, consonanats = List.partition 
                                (fun (x:string) -> match x.[0] with  
                                                     | 'a' | 'e' | 'i' | 'o' | 'u' -> true 
                                                     | _ -> false) arpabet


let a = beepUtils.take 20
