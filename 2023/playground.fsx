
let parseLine(input : string) : string * (int array) = 
    let (str, freq) = 
        match input.Split(" ") with
        | [|pattern1; pattern2 |] -> (
            pattern1,
            pattern2.Split(',') |> Array.map int)
        | _ -> failwith "incorrect input format"
    
    (String.concat "?" <| Array.replicate 1 str, Array.concat <| Array.replicate 1 freq)


let testPattern (input : string) (pattern : string) : bool = 
    (input.ToCharArray(), pattern.ToCharArray())
    ||> Array.forall2 (fun ch p ->
        match (ch, p) with
        | (_, '?') -> true
        | (a, b) -> a = b)

let combine (leftPadSize : int) (head : int) (rightPadSize : int) = 
    [Array.replicate leftPadSize "."; Array.replicate head "#"; Array.replicate rightPadSize "."] 
    |> Array.concat 
    |> String.concat ""

let rec generate (pattern : string) (freq : int list) (size : int) (startIndex : int) : string list =   
    let maxPadSize = size - (freq |> List.sum) - (freq.Length - 1)
    let minPadSize = if startIndex = 0 then 0 else 1

    let generate' = generate pattern

    

    match freq with
    | [] -> List.empty
    | [head] -> 
        [minPadSize..maxPadSize]
        |> List.map (fun padSize -> combine padSize head (size - (padSize + head)))
        |> List.filter (fun prefix -> testPattern prefix (pattern.Substring(startIndex, prefix.Length)))
    | head :: tail -> 
        [minPadSize..maxPadSize] 
        |> List.rev
        |> List.map (fun p -> (p, combine p head 0))
        |> List.filter (fun (_, prefix) -> testPattern prefix (pattern.Substring(startIndex, prefix.Length))) 
        |> List.map (fun (padSize, prefix) -> 
            let generated = generate' tail (size - (padSize + head)) (startIndex + padSize + head)
            
            generated |> List.map (fun g -> prefix + g))
        |> List.collect (fun s -> s)
    

let sw = System.Diagnostics.Stopwatch()

sw.Start()

let lines = @"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

lines.Split(System.Environment.NewLine)
    |> Array.map parseLine
    |> Array.iter(fun (pattern, freq) -> 
        let pattern' = pattern |> Array.replicate 5 |> String.concat "?"
        let freq' = freq |> Array.replicate 5 |> Array.concat |> Array.toList

        generate pattern' freq' pattern'.Length 0 |> List.length |> printfn "result %d"
    )

sw.Stop()
printfn "elapsed: %A" sw.Elapsed