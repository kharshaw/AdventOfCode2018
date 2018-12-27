open System
open System.IO

let readClaims filename =
  filename |> File.ReadLines |> List.ofSeq

let rawClaim = readBoxIds @"aoc-day3\data\data.txt"

type Claim = { Id:int; X:int; Y:int; Height:int; Width:int }

let parseRawClaim claimData = 


