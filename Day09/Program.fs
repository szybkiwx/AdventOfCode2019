// Learn more about F# at http://fsharp.org

open Common

let parseInstruction instr =
    (instr % 100,  (instr / 100) % 10 ,  (instr /1000) % 10, (instr / 10000) % 10)

let readArg arr idx mode relativeBase =
    match mode with
    | 0 -> let position = Array.get arr idx
           Array.get arr position
    | 1 -> Array.get arr idx
    | 2 -> let position = Array.get arr (idx + relativeBase)
           Array.get arr position
    | x -> failwith "error"
    
let calcNew arr i mode1 mode2 op relativeBase =
    let arg1 = readArg arr (i + 1) mode1 relativeBase
    let arg2 = readArg arr (i + 2) mode2 relativeBase
    let result = op arg1 arg2
    Array.set arr ( Array.get arr (i+3)) result 
    arr

let addOps a b = a + b
let mulOps a b = a * b

let op3 arr i input =
    let arg = Array.get arr (i+1)
    Array.set arr arg input
    arr
    
let op4 arr i mode1 =
    let arg = if mode1 = 0 then Array.get arr (Array.get arr (i+1)) else Array.get arr (i+1)
    (arg, arr)
  
let jumpIfTrue arr i mode1 mode2 relativeBase =
    let arg1 = readArg arr (i + 1) mode1 relativeBase
    let arg2 = readArg arr (i + 2) mode2 relativeBase
    
    if arg1 <> 0 then arg2 else i + 3
    
let jumpIfFalse arr i mode1 mode2 relativeBase =
    let arg1 = readArg arr (i + 1) mode1 relativeBase
    let arg2 = readArg arr (i + 2) mode2 relativeBase
    
    if arg1 = 0 then arg2 else i + 3
 
let lessThan arr i mode1 mode2 mode3 relativeBase =
    let arg1 = readArg arr (i + 1) mode1 relativeBase
    let arg2 = readArg arr (i + 2) mode2 relativeBase
    let arg3 = readArg arr (i + 3) 1 relativeBase
    
    if arg1 < arg2 then
        Array.set arr arg3 1
    else
        Array.set arr arg3 0 
    arr
    
let equals arr i mode1 mode2 mode3 relativeBase =
    let arg1 = readArg arr (i + 1) mode1 relativeBase
    let arg2 = readArg arr (i + 2) mode2 relativeBase
    let arg3 = readArg arr (i + 3) 1 relativeBase 
    
    if arg1 = arg2 then
        Array.set arr arg3 1
    else
        Array.set arr arg3 0 
    arr
    
let getNewBase arr i mode relativeBase=
    readArg arr (i + 1) mode relativeBase
    
let runProgram (program:int[]) (inputVals:int list) =
    let rec loop idx (arr:int[]) inp result relativeBase =
        let instruction = Array.get arr idx
        let (opcode, mode1, mode2, mode3) = parseInstruction instruction 
        match opcode with
        | 1 -> loop (idx + 4) (calcNew arr idx mode1 mode2 addOps relativeBase) inp result relativeBase
        | 2 -> loop (idx + 4) (calcNew arr idx mode1 mode2 mulOps relativeBase) inp result relativeBase
        | 3 -> match inp with
               | x::restInp -> loop (idx + 2) (op3 arr idx x) restInp result relativeBase
        | 4 -> let (output, newArr) = op4 arr idx mode1
               loop (idx + 2) newArr inp (output::result) relativeBase
        | 5 -> let nextIdx = jumpIfTrue arr idx mode1 mode2 relativeBase
               loop nextIdx arr inp result relativeBase
        | 6 -> let nextIdx = jumpIfFalse arr idx mode1 mode2 relativeBase
               loop nextIdx arr inp result relativeBase
        | 7  -> loop (idx + 4) (lessThan arr idx mode1 mode2 mode3 relativeBase) inp result relativeBase
        | 8  -> loop (idx + 4) (equals arr idx mode1 mode2 mode3 relativeBase) inp result relativeBase
        | 9  -> let newBase = getNewBase arr idx mode1 relativeBase
                loop (idx + 2) arr inp result (newBase + relativeBase) 
        | 99 -> result
        | _ -> failwith "Wrong op"
    let final = loop 0 program inputVals [] 0
    final.Head

[<EntryPoint>]
let main argv =
    let input = (readInput "input.txt").Split ',' |> Array.map (fun x -> x |> int)
    let result = runProgram input [1]
    0 // return an integer exit code
