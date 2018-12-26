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

let frequencies = Part1.boxIdLettersFrequencies boxIds
let part1 = Part1.checksum (Part1.frequencyOf 2 frequencies) (Part1.frequencyOf 3 frequencies)

