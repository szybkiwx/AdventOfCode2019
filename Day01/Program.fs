// Learn more about F# at http://fsharp.org

open System
open Common

let calcFuel mass =
    let fl = mass / 3.0 |> floor |> int
    fl-2

let day1 input = input |> Seq.map calcFuel |> Seq.sum

let calcMoreFuel mass =
    let rec loop (inputMass:float) result =
        let nextMass = float (calcFuel inputMass)
        if nextMass <= 0.0 then result else loop nextMass (result + nextMass) 
    loop mass 0.0  

let day2 input = input |> Seq.map calcMoreFuel |> Seq.sum
   


[<EntryPoint>]
let main argv =
    let input = readLines "input.txt" |> Seq.map float
    let result1 = day1 input 
    let result2 = day2 input 
    
    //let d = calcMoreFuel 100756.0
    
    printfn "Hello World from F#!"
    0 // return an integer exit code
