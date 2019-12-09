// Learn more about F# at http://fsharp.org

open Common

let makeLayers (input:int[]) width height =
    input |> Array.chunkBySize (width * height)

let part1 input =
    let layers = makeLayers input 25 6
    let res = layers |> Array.map (fun layer -> Array.filter (fun x -> x = 0) layer |> Array.length )
    let minVal = res |> Array.minBy id
    let idx = res |> Array.findIndex (fun x -> x = minVal)
    let ones = layers.[idx] |> Array.filter (fun x -> x = 1) |> Array.length
    let twos = layers.[idx] |> Array.filter (fun x -> x = 2) |> Array.length
    ones * twos
    
let part2 input =
    let layers = makeLayers input 25 6
    let resultingImage = [for pixelNo in 0..(25*6)-1 do
                            let pixelsFromLayers = [for layer in layers -> layer.[pixelNo]]
                            let rec loop remainingLayers =
                                match remainingLayers with
                                | pixel::rest -> match pixel with
                                                 | 2 -> loop rest
                                                 | x -> x
                                | [] -> failwith "error"
                            yield loop pixelsFromLayers ]            
                
    resultingImage
                

    
[<EntryPoint>]
let main argv =
    let input = readInput "input.txt" |> Seq.map (fun x -> x |> string |> int) |> Seq.toArray
    //let res = part1 input
    let res2 = part2 input
    for i in 0..6 do
        for j in 0..24 do
            let pixel = res2.[i * 25 + j]
            if pixel = 1 then
                printf "O"
            else printf " "
        printf "\n"    
    0 // return an integer exit code
