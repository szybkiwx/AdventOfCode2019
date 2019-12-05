// Learn more about F# at http://fsharp.org

open System

let int2list arg =
    let rec loop remaining result =
        match remaining with
        | 0 -> result
        | x -> loop (x / 10) ((x % 10)::result)
    loop arg []

let isValid1 (password:int) =
    let rec loop digitsLeft last hasDouble =
        match digitsLeft with
        | current::rest -> if current >= last then loop rest current (hasDouble ||  current = last) else false
        | [] -> hasDouble
    
    loop (int2list password) -1 false


let isValid2 (password:int) =
    let rec loop digitsLeft last =
        match digitsLeft with
        | current::rest -> if current >= last then loop rest current else false
        | [] -> true
    let lst = (int2list password) 
    isValid1 password && (List.groupBy id lst |> List.map (fun (_, occurances) -> List.length occurances ) |> List.contains 2)
        

let checkPasswords isValid min max =
    [for i in min..max do if isValid i then yield i] |> List.length

[<EntryPoint>]
let main argv =
    let x = checkPasswords isValid1 123257 647015
    let y = checkPasswords isValid2 123257 647015
    
    
    0 // return an integer exit code
