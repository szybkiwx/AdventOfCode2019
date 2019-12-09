// Learn more about F# at http://fsharp.org

open Common
open Day05

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let runAmps program phasesInit =
    let rec loop phases prev =
        match phases with
        | phase::rest -> let input = [phase; prev]
                         let output = runProgram program input
                         loop rest output
        | [] -> prev

    loop phasesInit 0
let part1 program =
    let phases = permute [for i in 0..4 -> i]
    let rec loop phaseList outputs =
        match phaseList with
        | phases::rest -> let output = runAmps program phases
                          loop rest (output::outputs)
        | [] -> outputs
    
    let outputs = loop phases []    
    outputs |> List.max

let part2 program =
    let phases = permute [for i in 5..9 -> i]
    let rec loop phaseList outputs =
        match phaseList with
        | phases::rest -> let output = runAmps program phases
                          loop rest (output::outputs)
        | [] -> outputs
    
    let outputs = loop phases []    
    outputs |> List.max

    
[<EntryPoint>]
let main argv =
    let program = (readInput "input.txt").Split ',' |> Array.map (fun x -> x |> int)
    //let program = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0".Split ',' |> Array.map (fun x -> x |> int)
    //let output = runAmps program [1;0;4;3;2]
    
    let result1 = part1 program
    printfn "Hello World from F#!"
    0 // return an integer exit code
