// Learn more about F# at http://fsharp.org

open Common


let calcNew arr i op =
    let arg1 = Array.get arr (Array.get arr (i+1))
    let arg2 = Array.get arr (Array.get arr (i+2))
    let result = op arg1 arg2
    Array.set arr ( Array.get arr (i+3)) result
    arr

let addOps a b = a + b
let mulOps a b = a * b

let runProgram (input:int[]) pos1 pos2 =
    Array.set input 1 pos1
    Array.set input 2 pos2
    let rec loop idx (arr:int[]) =
        let opcode = Array.get arr idx
        match opcode with
        | 1 -> loop (idx+4) (calcNew arr idx addOps) 
        | 2 -> loop (idx+4) (calcNew arr idx mulOps) 
        | 99 -> arr
        | _ -> failwith "Wrong op"
    let result = loop 0 input
    //for i in result do
    //     printf "%d, " i |> ignore
    Array.get result 0

let day1 (input:int[]) =
    runProgram input 12 2

let day2 (input:int[]) =
    [for i in 0..99 do for j in 0..99 -> (i, j) ] |> List.map (fun (i, j) ->
        let arr = Array.copy input
        let result = runProgram arr i j
        (i, j, result)) |> List.filter (fun (i, j, result) -> result = 19690720) |> List.head |> fun(i, j, result) -> (i, j)


[<EntryPoint>]
let main argv =
    let input = (readInput "input.txt").Split ',' |> Array.map (fun x -> x |> int)
    //let result1 = day1 input 
    let result2 = day2 input
    0 // return an integer exit code
