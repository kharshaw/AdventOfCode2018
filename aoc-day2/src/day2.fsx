open System
open System.IO

let readBoxIds filename =
  filename |> File.ReadLines |> List.ofSeq

let boxIds = readBoxIds @"aoc-day2\data\data.txt"


// let boxIds = [
//   "aabcde"
//   "abbccce"
//   "bbbcccd"
//   ]


let alphaList = Seq.toList "abcdefghijklmnopqrstuvwxyz"

module Part1 =
  let charCountInString chr str =
    Seq.toList str |> List.filter (fun x -> string x = string chr) |> List.length

  let boxIdLettersFrequencies boxIds = (List.map (fun boxId -> (
      List.map ( fun alpha -> charCountInString alpha boxId) alphaList
      )
    ) boxIds)

  let hasOccuranceOf occurance boxIdLettersFrequency =
    match List.contains occurance boxIdLettersFrequency with
    | true -> 1
    | false -> 0


  let frequencyOf x frequencies = (List.map (fun boxIdLettersFrequency -> (
    hasOccuranceOf x boxIdLettersFrequency)
    ) frequencies)

  let checksum x y = (List.sum x) * (List.sum y)

module Part2 =

  let deltaMask str1 str2 = (List.map2 (fun c1 c2 -> (
      match c1 = c2 with
      | true -> 0
      | false -> 1
      )
    ) (Seq.toList str1) (Seq.toList str2))


  let rec findFirstCandidateWithDifferenceX model differences candidates =
    let masks = List.map (fun c -> deltaMask model c) candidates

    match (List.tryFindIndex ( fun m -> List.sum m = differences ) masks) with
    | Some idx -> Some ((List.item idx candidates), (List.item idx masks))
    | None when List.length candidates > 1 -> findFirstCandidateWithDifferenceX (List.head candidates) differences (List.skip 1 candidates)
    | None -> None


  let rec findFirstCandidateWithSmallestDifference differences candidates =
    let model = List.head candidates
    let candidates' = List.skip 1 candidates
    printfn "%A diff: %A candidates: %A" model differences candidates'

    match (findFirstCandidateWithDifferenceX model differences candidates') with
    | Some (x, y) -> Some (x, y)
    | None when differences < List.length candidates' -> findFirstCandidateWithSmallestDifference (differences+1) candidates
    | None -> None

  let stripMasked box mask = (
    let matchBoxZipped = List.zip box mask |> List.filter (fun (x,y) -> y = 0 )
    let box', mask' = List.unzip matchBoxZipped
    box'
    )

  let mostCommonBoxId boxIds = (
      let boxMask = findFirstCandidateWithSmallestDifference 0 boxIds
      let box, mask = boxMask.Value
      stripMasked (Seq.toList box) mask
    )

let frequencies = Part1.boxIdLettersFrequencies boxIds

let part1 = Part1.checksum (Part1.frequencyOf 2 frequencies) (Part1.frequencyOf 3 frequencies)


let part2 = Part2.mostCommonBoxId boxIds

System.String.Concat(Array.ofList(part2))