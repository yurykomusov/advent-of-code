open System
open System.IO

let parseLine(input : string) : string * (int array) = 
    let (str, freq) = 
        match input.Split(" ") with
        | [|pattern1; pattern2 |] -> (
            pattern1,
            pattern2.Split(',') |> Array.map int)
        | _ -> failwith "incorrect input format"
    
    (String.concat "?" <| Array.replicate 1 str, Array.concat <| Array.replicate 1 freq)

let testPattern (padLeft : int) (head : int) (padRight : int) (pattern : char array) (startIndex : int) = 
    let mutable result = true
    for i in 0..(padLeft + head + padRight) - 1 do
        let ch = 
            match i with
            | ii when ii < padLeft -> '.'
            | ii when ii < head + padLeft -> '#'
            | ii -> '.'

        result <- result && 
            match (ch, pattern[startIndex + i]) with
            | (_, '?') -> true
            | (a, b) -> a = b
    result

let cache = new System.Collections.Generic.Dictionary<int * int, int64>()

let rec generate (pattern : char array) (freq : int list) (size : int) (startIndex : int) : int64 =   
    let maxPadSize = size - (freq |> List.sum) - (freq.Length - 1)
    let minPadSize = if startIndex = 0 then 0 else 1

    let generate' = generate pattern

    let mutable sum : int64 = 0L 

    match cache.TryGetValue((startIndex, freq.Length)) with
    | true,value -> value
    | false,_ ->
        let result = 
            match freq with
            | [] -> 0L
            | [head] -> 
                    [minPadSize..maxPadSize]
                    |> List.filter (fun padSize -> 
                        let padLeft = padSize
                        let padRight = (size - (padSize + head))
                        testPattern padLeft head padRight pattern startIndex)
                    |> List.length
                    |> int64 
            | head :: tail -> 
                [minPadSize..maxPadSize] 
                |> List.filter (fun padSize -> 
                    let padLeft = padSize
                    let padRight = 0
                    testPattern padLeft head padRight pattern startIndex) 
                |> List.iter (fun padSize -> 
                    sum <- sum + (generate' tail (size - (padSize + head)) (startIndex + padSize + head)))
                sum
                    
        cache.Add((startIndex, freq.Length), result)
        result

let path = Path.Combine(__SOURCE_DIRECTORY__, "Day12.txt")
let lines = File.ReadAllLines(path)


let mutable result : int64 = 0L

lines
    |> Array.map parseLine
    |> Array.iter(fun (pattern, freq) -> 
        cache.Clear()
        let pattern' = pattern |> Array.replicate 5 |> String.concat "?"
        let freq' = freq |> Array.replicate 5 |> Array.concat |> Array.toList

        result <- result + (generate (pattern'.ToCharArray()) freq' pattern'.Length 0)
    )
printfn "result %d" result