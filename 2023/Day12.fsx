open System
open System.IO

let rec indexes (size: int) (n : int) (start : int) : int seq seq = 
    // System.Console.WriteLine($"size - {size} n - {n} start - {start}")
    seq {
        if n > 0 then
            for i in start..size - 1 do
                let start' = i + 1
                let arr = seq { 
                    let inner = indexes size (n - 1) start' -> ([i] @ j) 
                }
                
                if Seq.isEmpty arr then
                    yield [i]
                else
                    for ii in arr do
                        yield ii
    }
    //|> Seq.filter (fun i -> List.length i = n)

let generateCombination (size : int) (indexes : int list) : char array =
    let result = Array.init size (fun i -> 
        match indexes |> (List.tryFind (fun ii -> ii = i)) with
        | Some _ -> '#'
        | None -> '.' )
    result

let generateCombinations (size : int) (n : int) = 
    indexes size n 0 |> Seq.map (generateCombination size)

let test (input : string) (testArr: int array) = 
    let transformed = 
        input.Split('.', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map(fun i -> i.Length)
    
    if transformed.Length <> testArr.Length then
        false
    else
        (transformed, testArr) ||> Array.forall2 (fun a b -> a = b)
    // ###..## -> [3, 2]


// let combination = 
//     (indexes 4 2 0)
//     |> List.map (generateCombination 4)
//     |> List.head

let heal (combo : char array) (damaged: string) = 
    let arr = damaged.ToCharArray()
    let missingIndexes = 
        arr 
        |> Array.indexed 
        |> Array.filter(fun (idx, c) -> c = '?') 
        |> Array.map (fun (idx, c) -> idx)

    for i in 0..missingIndexes.Length - 1 do
        arr[missingIndexes[i]] <- combo[i]
    arr

let parseLine(input : string) : string * (int array) = 
    let (str, freq) = 
        match input.Split(" ") with
        | [|pattern1; pattern2 |] -> (
            pattern1,
            pattern2.Split(',') |> Array.map int)
        | _ -> failwith "incorrect input format"
    
    (String.concat "?" <| Array.replicate 4 str, Array.concat <| Array.replicate 4 freq)


// calculate how many # and . expected from input
let findHints (str : string) (frequencies : (int array)) =
    let countMissing = str.ToCharArray() |> Array.filter (fun i -> i = '?') |> Array.length
    let countFoundDamaged = str.ToCharArray() |> Array.filter (fun i -> i = '#') |> Array.length
    let countAllDamaged = frequencies |> Array.sum
    let countMissingDamaged = countAllDamaged - countFoundDamaged

    (countMissingDamaged, countMissing)
    

async {
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    let path = Path.Combine(__SOURCE_DIRECTORY__, "Day12.txt"); 
    let! lines = File.ReadAllLinesAsync(path) |> Async.AwaitTask

    let lines = [| ".??..??...?##. 1,1,3" |]
    
    lines
    |> Array.map parseLine
    |> Array.map (
        fun (damagedStr, frequencies) -> 
            let (missingDamaged, missingAll) = findHints damagedStr frequencies
            let combinations = generateCombinations missingAll missingDamaged |> Seq.cache

            printfn $"possibly damaged: {missingDamaged} of {missingAll}"
            printfn $"combinations : {combinations |> Seq.length}"

            let compatibleCombinations = 
                combinations 
                |> Seq.filter (fun combo -> 
                    let healed = (heal combo damagedStr) |> String.Concat
                    let compatible = test healed frequencies
                   
                    //if compatible then
                    // printfn "testing : %s for %A -> %b" healed frequencies compatible

                    compatible)
            
            // printfn "%A: %d" frequencies (compatibleCombinations |> Seq.length)
            
            // for cc in compatibleCombinations do
            //     Console.ReadLine() |> ignore
            //     printfn "%s" (String.Concat(cc))
                
            let result = Seq.length compatibleCombinations

            printfn "%A - %d" frequencies result

            result)
    |> Array.sum
    |> printfn "result: %d"

//Warning. known bug for ?#.#?.?#?? [|1; 1; 1|]: 0 - should be 1

//|> Array.iter (printfn "number of successful subs: %A")

    sw.Stop()

    printfn $"elapsed: {sw}"

} |> Async.RunSynchronously

