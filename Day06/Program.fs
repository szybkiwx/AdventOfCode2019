// Learn more about F# at http://fsharp.org

open Common
open System.Collections.Generic

let buildTree (input:(string*string) list) =
    input |> List.groupBy fst |> List.map (fun (src, lst) -> (src, lst|> List.map snd )) |> Map.ofList
    
let printPath (path:(string list)) =
    for i in path do printf "%s->" i
    printf "\n"

let part1 (input:(string*string) list) =
    let tree = buildTree input
    let distances = new Dictionary<string, int>()
    let rec findDistances current currentDist =
        let newDistance = currentDist + 1
        distances.Add(current, currentDist)
        if (Map.containsKey current tree) then
            for adj in tree.[current] do
                findDistances adj newDistance
                
    findDistances "COM" 0
    distances.Values |> Seq.sum
  
[<EntryPoint>]
let main argv =
    let input = readLines "input.txt" |> Seq.map (fun line -> let tokens = line.Split ')'
                                                              (tokens.[0], tokens.[1]) ) |> Seq.toList
    //let x = buildTree input;
    let result = part1 input
    0 // return an integer exit code
