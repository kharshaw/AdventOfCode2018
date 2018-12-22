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

let runningTotals seq =
  (Seq.head seq, Seq.skip 1 seq) ||> Seq.scan (+)

let rec findFirstDup idx foundNetFrequencies sqnc =
  let currentNet = Seq.item idx sqnc
  let idx' = idx + 1
  let foundNetFequencies' = foundNetFrequencies |> Set.add currentNet
  let exists = Set.contains currentNet foundNetFrequencies
  match exists with
  | true -> currentNet
  | false -> findFirstDup idx' foundNetFequencies' sqnc


let printDuplicate dup =
  match dup with
  | Some value -> string value
  | None -> "none"

let repeat cycle =
  seq { while true do yield! cycle}


let firstChallenge frequencies =
  Seq.sum(frequencies)

let secondChallenge frequencyCycle =
  let frequencyNet = frequencyCycle |> repeat |> runningTotals

  frequencyNet |> findFirstDup 0 Set.empty

[<EntryPoint>]
let main argv =
    let frequencyCycle = readFrequencies @"data\input.txt"

    printfn "Sum: %A" (firstChallenge frequencyCycle)

    printfn "First duplicate: %A" (secondChallenge frequencyCycle)

    0 // return an integer exit code
