let boxIds = [
  "aabcde"
  "abbccde"
  "bbbcccd"
  ]


let alphaList = Seq.toList "abcdefghijklmnopqrstuvwxyz"

let charCountInString chr str =
  Seq.toList str |> List.filter (fun x -> string x = string chr) |> List.length

(List.map (fun boxId -> (
    List.map ( fun alpha -> charCountInString alpha boxId) alphaList
  )
) boxIds)

charCountInString 'a' "aaa"
charCountInString 'x' "aaa"