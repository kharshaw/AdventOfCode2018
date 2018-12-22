open System
open System.IO


let (|Int|_|) (str : String) =
  match Int32.TryParse(str) with
  | (true,int) -> Some(int)
  | _ -> None

let parseFrequency line =
  match line with
  | Int i -> i
  | _ ->  0

let runningTotal seq =
  (Seq.head seq, Seq.skip 1 seq) ||> Seq.scan (+)

let rec findDup idx foundNetFrequencies sqnc =
  let currentNet = Seq.item idx sqnc
  let idx' = idx + 1
  let foundNetFequencies' = foundNetFrequencies |> Set.add currentNet
  let exists = Set.contains currentNet foundNetFrequencies
  match exists with
  | true -> currentNet
  | false -> findDup idx' foundNetFequencies' sqnc

let printDuplicate dup =
  match dup with
  | Some value -> string value
  | None -> "none"

let frequencyCycle = [ 1; 2; -4;] :> seq<int>
let s = Seq.item 1 frequencyCycle
let sumOfSeq = Seq.sum frequencyCycle


let repeat cycle =
  seq { while true do yield! cycle}



let frequencyNet = frequencyCycle |> repeat |> runningTotal

let firstDup = findDup 0 Set.empty frequencyNet

printfn "First repeat: %A" firstDup