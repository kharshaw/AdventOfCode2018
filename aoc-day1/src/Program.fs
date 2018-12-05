// Learn more about F# at http://fsharp.org

open System
open System.IO

let (|Int|_|) (str : String) =
  match Int32.TryParse(str) with
  | (true,int) -> Some(int)
  | _ -> None

let parseFrequency line =
  match line with
  | Int i -> i
  | _ -> 0

let readFrequencies filename =
  filename |> File.ReadLines |> Seq.map parseFrequency

[<EntryPoint>]
let main argv =
    let frequencies = readFrequencies @"data\input.txt"
    let net = Seq.sum(frequencies)

    printfn "Sum: %A" net
    0 // return an integer exit code
