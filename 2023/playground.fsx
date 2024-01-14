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


let combine (leftPadSize : int) (head : int) (rightPadSize : int) = 
    let len = leftPadSize + head + rightPadSize
    Array.init len (fun index -> 
        match index with
        | a when a < leftPadSize -> '.'
        | a when a >= leftPadSize && a < leftPadSize + head -> '#'
        | _ -> '.')
    
let rec generate (pattern : char array) (freq : int list) (size : int) (startIndex : int) : int =   
    let maxPadSize = size - (freq |> List.sum) - (freq.Length - 1)
    let minPadSize = if startIndex = 0 then 0 else 1

    let generate' = generate pattern

    match freq with
    | [] -> 0
    | [head] -> 
        [minPadSize..maxPadSize]
        |> List.map (fun padSize -> combine padSize head (size - (padSize + head)))
        |> List.filter (fun prefix -> testPattern prefix pattern startIndex)
        |> List.length
    | head :: tail -> 
        [minPadSize..maxPadSize] 
        |> List.rev
        |> List.map (fun p -> (p, combine p head 0))
        |> List.filter (fun (_, prefix) -> testPattern prefix pattern startIndex) 
        |> List.map (fun (padSize, prefix) -> 
            generate' tail (size - (padSize + head)) (startIndex + padSize + head)
        )
            //generated |> List.map (fun g -> prefix + g))
        //|> List.collect (fun s -> s)
        |> List.sum
    

let sw = System.Diagnostics.Stopwatch()

sw.Start()

let path = Path.Combine(__SOURCE_DIRECTORY__, "Day12.txt")
let lines = File.ReadAllLines(path)

// let lines = @"???.### 1,1,3
// .??..??...?##. 1,1,3
// ?#?#?#?#?#?#?#? 1,3,1,6
// ????.#...#... 4,1,1
// ????.######..#####. 1,6,5
// ?###???????? 3,2,1"

lines
    |> Array.map parseLine
    |> Array.iter(fun (pattern, freq) -> 
        let pattern' = pattern |> Array.replicate 4 |> String.concat "?"
        let freq' = freq |> Array.replicate 4 |> Array.concat |> Array.toList

        generate (pattern'.ToCharArray()) freq' pattern'.Length 0 |> printfn "result %d"
    )

sw.Stop()
printfn "elapsed: %A" sw.Elapsed