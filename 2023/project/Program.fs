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


let testPattern (input : char array) (pattern : char array) (startIndex : int) : bool = 
    let mutable result = true
    for i in 0..input.Length - 1 do
        let ch = input[i]
        let p = pattern[startIndex + i]
        result <- result &&
            match (ch, p) with
            | (_, '?') -> true
            | (a, b) -> a = b
    result


let testPattern2 (padLeft : int) (head : int) (padRight : int) (pattern : char array) (startIndex : int) = 
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
    
let rec generate (pattern : char array) (freq : int list) (size : int) (startIndex : int) : int =   
    let maxPadSize = size - (freq |> List.sum) - (freq.Length - 1)
    let minPadSize = if startIndex = 0 then 0 else 1

    let generate' = generate pattern

    match freq with
    | [] -> 0
    | [head] -> 
        [minPadSize..maxPadSize]
        |> List.filter (fun padSize -> 
            let padLeft = padSize
            let padRight = (size - (padSize + head))
            testPattern2 padLeft head padRight pattern startIndex)
        |> List.length
    | head :: tail -> 
        [minPadSize..maxPadSize] 
        |> List.filter (fun padSize -> 
            let padLeft = padSize
            let padRight = 0
            testPattern2 padLeft head padRight pattern startIndex) 
        |> List.sumBy (fun padSize -> 
            generate' tail (size - (padSize + head)) (startIndex + padSize + head))

let sw = System.Diagnostics.Stopwatch()

sw.Start()

let path = Path.Combine(Environment.CurrentDirectory, "Day12.txt")
let lines = File.ReadAllLines(path)

// let lines = Array.singleton @"?.????????#??? 1,2,2"

lines
    |> Array.map parseLine
    |> Array.iter(fun (pattern, freq) -> 
        let pattern' = pattern |> Array.replicate 4 |> String.concat "?"
        let freq' = freq |> Array.replicate 4 |> Array.concat |> Array.toList

        generate (pattern'.ToCharArray()) freq' pattern'.Length 0 |> Console.WriteLine
    )

sw.Stop()
printfn "elapsed: %A" sw.Elapsed