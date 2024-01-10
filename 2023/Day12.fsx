open System
open System.IO

let rec getCombinationIndexes (size: int) (n : int) (start : int) : int seq seq = 
    // System.Console.WriteLine($"size - {size} n - {n} start - {start}")
    seq {
        if n > 0 then
            for i in start..size - 1 do
                let arr = getCombinationIndexes size (n - 1) (i + 1)
                
                if Seq.isEmpty arr && n = 1 then
                    yield seq { yield i }
                else
                    for ii in arr do
                        yield Seq.append (seq { yield i }) ii
    }
    //|> Seq.filter (fun i -> List.length i = n)

let getDamageIndexes (input : char array) =
    input 
    |> Array.indexed
    |> Array.filter (fun (index, element) -> element = '#')
    |> Array.map (fun (index, element) -> index)

let getMissingIndexes (input : char array) =
    input 
    |> Array.indexed
    |> Array.filter (fun (index, element) -> element = '?')
    |> Array.map (fun (index, element) -> index)



let applyFoundDamages (indexes : int array) (damageIndexes : int array) =
    Array.concat [indexes; damageIndexes]
let fixIndexes (indexes : int array) (missing : int array) =
    //printf "fixIndexes %A - %A" indexes missing 
    let result = indexes |> Array.map (fun idx -> missing[idx])
    //printfn "; result - %A" result
    result 

let foldDamages (indexes : int array) =
    indexes 
    |> Array.sort
    |> Array.mapi (fun index element -> index - element)
    |> Array.countBy (fun i -> i)
    |> Array.map (fun (key, count) -> count)

let test (possible : int array) (expected: int array) = 
    if possible.Length <> expected.Length then
        false
    else
        (possible, expected) ||> Array.forall2 (fun a b -> a = b)
    // ###..## -> [3, 2]

let parseLine(input : string) : string * (int array) = 
    let (str, freq) = 
        match input.Split(" ") with
        | [|pattern1; pattern2 |] -> (
            pattern1,
            pattern2.Split(',') |> Array.map int)
        | _ -> failwith "incorrect input format"
    
    (String.concat "?" <| Array.replicate 1 str, Array.concat <| Array.replicate 1 freq)


// calculate how many # and . expected from input
let findHints (charArr : char array) (frequencies : (int array)) =
    let countMissing = charArr |> Array.filter (fun i -> i = '?') |> Array.length
    let countFoundDamaged = charArr |> Array.filter (fun i -> i = '#') |> Array.length
    let countAllDamaged = frequencies |> Array.sum
    let countMissingDamaged = countAllDamaged - countFoundDamaged

    (countMissingDamaged, countMissing)
    

async {
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    let path = Path.Combine(__SOURCE_DIRECTORY__, "Day12.txt"); 
    let! lines = File.ReadAllLinesAsync(path) |> Async.AwaitTask

    // let lines = [| ".??..??...?##. 1,1,3" |]
    
    lines
    |> Array.map parseLine
    |> Array.map (
        fun (damagedStr, frequencies) -> 
            let damagedStrCharArr = damagedStr.ToCharArray()
            let (missingDamaged, missingAll) = findHints damagedStrCharArr frequencies
            let indexesSeq = getCombinationIndexes missingAll missingDamaged 0
            let foundDamageIndexes = getDamageIndexes damagedStrCharArr
            let foundMissingIndexes = getMissingIndexes damagedStrCharArr

            let mutable positive = 0
            let mutable counter = 0


            for indexes in indexesSeq do
                let indexArr = indexes |> Seq.toArray

                let allDamage = applyFoundDamages (fixIndexes indexArr foundMissingIndexes) foundDamageIndexes
                let testFrequencies = foldDamages allDamage

                if test testFrequencies frequencies then
                    positive <- positive + 1

                counter <- counter + 1 

                if counter % 10000 = 0 then
                    printfn "over 10000"
            positive
        )
    |> Array.sum
    |> printfn "result: %d"

//Warning. known bug for ?#.#?.?#?? [|1; 1; 1|]: 0 - should be 1

//|> Array.iter (printfn "number of successful subs: %A")

    sw.Stop()

    printfn $"elapsed: {sw}"

} |> Async.RunSynchronously

