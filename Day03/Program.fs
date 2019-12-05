// Learn more about F# at http://fsharp.org

open Common

let mdist (x1, y1) (x2, y2) =  (abs (x2 - x1)) + (abs (y2 - y1))

let writeWire (instructions:(char*int)[]) =
    let rec followInstruction instructionsLeft pointer wire =
        match instructionsLeft with
        | (dir, dist)::rest -> let (dx, dy) = match dir with
                                                | 'R' -> (1, 0)
                                                | 'L' -> (-1, 0)
                                                | 'U' -> (0, 1)
                                                | 'D' -> (0, -1)
                                                | _ -> failwith "error"
                               let (px, py) = pointer
                               let newSegment = [for i in 0..dist-1 -> (px + dx * i, py + dy * i)]
                               followInstruction rest (px + dx*dist, py + dy*dist) (wire@newSegment) 
            
        | [] -> wire
        
    followInstruction (Array.toList instructions) (0, 0) []
        
let findMinCrossing (wire1:(int*int) list) (wire2:(int*int) list) =
    let crossing = Set.intersect (Set.ofList wire1) (Set.ofList wire2)
    Set.remove (0, 0) crossing |> Set.map (fun x -> (x, mdist x (0, 0))) |> Set.toList |> List.minBy snd  //|> List.groupBy snd 

let day1 (set1:(char*int)[]) (set2:(char*int)[]) =
    let wire1 = writeWire set1
    let wire2 = writeWire set2
    snd (findMinCrossing wire1 wire2) 


let countSteps (wire:(int*int) list) (crossings:(int*int) list)
    = List.map (fun cross -> (cross, (List.findIndex (fun x -> x = cross) wire))) crossings |> Map.ofList

let findClosestCrossing (wire:(int*int) list) (crossings:(int*int) list) =
    crossings |> List.map (fun x -> List.findIndex (fun step -> step = x) wire) |> List.min 

(*let day2 (set1:(char*int)[]) (set2:(char*int)[]) =
    let wire1 = writeWire set1
    let wire2 = writeWire set2
    
    let crossings = Set.intersect (Set.ofList wire1) (Set.ofList wire2) |> Set.remove (0, 0) |> Set.toList
        
    let step1 = findClosestCrossing wire1 crossings 
    let step2 = findClosestCrossing wire2 crossings
    step1 + step2*)

let day2 (set1:(char*int)[]) (set2:(char*int)[]) =
    let wire1 = writeWire set1
    let wire2 = writeWire set2
    
    let crossings = Set.intersect (Set.ofList wire1) (Set.ofList wire2) |> Set.remove (0, 0) |> Set.toList
    let steps1 = countSteps wire1 crossings
    let steps2 = countSteps wire2 crossings
    
    let steps = [for cross in crossings do
                    let cnt1 =  steps1.[cross]
                    let cnt2 =  steps2.[cross]
                    yield cnt1 + cnt2] 
    steps |> List.min    
    
    
    
    
    
    
    
    
    

[<EntryPoint>]
let main argv =
    let input = readLines "input.txt" |> Seq.map(fun x -> x.Split ',') |> Seq.map(fun x -> x |> Array.map (fun x -> (x.[0], int x.[1..]))) |> Seq.toArray
    //let test1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72".Split ',' |> Array.map (fun x -> (x.[0], int x.[1..]))
    //let test2 = "U62,R66,U55,R34,D71,R55,D58,R83".Split ',' |> Array.map (fun x -> (x.[0], int x.[1..]))
    //let wire1 = writeWire test1
    //let wire2 = writeWire test2
    //let result = findMinCrossing wire1 wire2 
    
    //let result = day1 test1 test2 
    //let result2 = day2 test1 test2 
    
    
    //let result = day1 input.[0] input.[1]
    let result2 = day2 input.[0] input.[1]
    0 // return an integer exit code
