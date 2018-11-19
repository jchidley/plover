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

let beepFile = @"D:\downloads\beep.tar\beep\beep-1.0"
// @"S:\DadOnly\Downloads\beep.tar\beep\beep-1.0"
// @"D:\downloads\beep.tar\beep\beep-1.0"

let b = System.IO.File.ReadLines(beepFile)

let trimmed = b 
              |> Seq.skipWhile 
                (fun (x:string) ->  let y = x.Trim()
                                    y.StartsWith "#")

let beepSorter (x:seq<string>) = (Seq.head x).[1..] 

let splitItUp (x:string) = 
  x.Split() 
  |> Seq.choose (fun x ->
                    match x with
                      | "" -> None
                      | _ -> Some(x) )
  |> Seq.toList                    

Seq.take 20 trimmed |> Seq.map splitItUp |> Seq.sortBy beepSorter
