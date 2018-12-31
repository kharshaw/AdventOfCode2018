open System
open System.IO

let readClaims filename =
  filename |> File.ReadLines |> List.ofSeq

let rawClaims = readClaims @"aoc-day3\data\data.txt"

type Claim = { Id:int; X:int; Y:int; Height:int; Width:int }
type Point = { X:int; Y:int }

let parseRawClaim (claimData:string) = (
  let valuesString = claimData.Replace("#", "")
                              .Replace(" @ ", ",")
                              .Replace(": ", ",")
                              .Replace("x",",")
  let valuesArray = valuesString.Split(',') |> Array.map (fun s -> int s)

  let claim = {Id = valuesArray.[0]; X = valuesArray.[1]; Y = valuesArray.[2]; Height = valuesArray.[4]; Width = valuesArray.[3]}

  claim
  )

let claims = rawClaims |> Seq.map (fun data -> parseRawClaim data )

let clothWidth = 1200
let clothHeight = 1200

let getPointFromIndex width height idx =
  let y = idx / width
  let x = idx % width
  { X = x; Y = y }

let isPointInClaim point (claim: Claim) =
  (point.X >= claim.X && point.X <= (claim.X + claim.Width - 1) && point.Y >= claim.Y && point.Y <= (claim.Y + claim.Height - 1))

let placeClaimOnCloth width height claim  =
  let claimMatrix = [ for i in 0 .. ((clothWidth*clothWidth)-1) -> (
    match (isPointInClaim (getPointFromIndex width height i) claim) with
    | true -> 1
    | false -> 0
    )]
  claimMatrix

let claimLocations width height claims =
    claims |> Seq.map (fun c -> placeClaimOnCloth width height c)


let addPlacedClaims placedClaim1 placedClaim2 =
  Seq.map2 (fun c1 c2 -> c1 + c2) placedClaim1 placedClaim2


let addAllPlacedClaims claims =
 (Seq.head claims, Seq.skip 1 claims) ||> Seq.fold (combinedPlacedClaims)


let claims' = claims |> claimLocations clothWidth clothHeight

let cloth =  claims' |> addAllPlacedClaims


let isNoOverlapMatch claimPositions cloth =
  Seq.map2 (fun claim cloth ->
    match (claim, cloth) with
    | (1,1) -> true
    | (0,_) -> true
    | (_,_) -> false
  ) claimPositions cloth


let part1 clothWithClaims =
  cloth |> Seq.filter (fun c -> c > 1) |> Seq.length


let part2 cloth claims  =
  let overlapMap = claims |> Seq.map (fun claim -> isNoOverlapMatch claim cloth  )
  let firstNonOverlap = overlapMap |> Seq.findIndex (fun map -> Seq.forall (fun el -> el = true) map)
  firstNonOverlap



part1 cloth

part2 cloth claims'