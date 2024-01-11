open System
open System.IO

let sw = new System.Diagnostics.Stopwatch()
sw.Start()

let parseLine(input : string) : string * (int array) = 
    let (str, freq) = 
        match input.Split(" ") with
        | [|pattern1; pattern2 |] -> (
            pattern1,
            pattern2.Split(',') |> Array.map int)
        | _ -> failwith "incorrect input format"
    
    (String.concat "?" <| Array.replicate 1 str, Array.concat <| Array.replicate 1 freq)

let testExact (input : string) (freqExpected : int array) : int = 
    match (input, freqExpected) with
    | ("", [||]) -> 1
    | ("", arr) -> 0
    | ("#", [|1|]) -> 1
    | ("#", [|0|]) -> 0
    | (s, freq) -> 
        let freqActual = 
            input.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map _.Length

        if freqActual.Length = freqExpected.Length && ((freqActual, freqExpected) ||> Array.forall2 (fun a b -> a = b)) then
            1
        else
            0


let chunks (input : string) = 
    input.Split('.', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map _.Length


let validatePartial (chunks : int array) (freq : int array) =
        let actual = chunks |> Array.indexed
        
        let actualLen = chunks.Length
        let freqLen = freq.Length

        if actualLen > freqLen then
            false
        else
            actual
            |> Array.forall (fun (index, element) -> 
                    match index with
                    | s when s < actualLen - 1 -> element = freq[index]
                    | _ -> index < freqLen && element <= freq[index])

let validateSum (maxProjectedCount : int) (controlSum : int) =
    maxProjectedCount >= controlSum

let rec test (input : string) (frequencies : int array) (controlSum : int) (maxProjectedCount : int) : int =         
    if validateSum maxProjectedCount controlSum = false
        then 0
    else
        let result = 
            match input.Split('?', 2) with
            | [| str |] -> testExact str frequencies
            | [| str1; str2 |] -> 
                let ch = chunks str1
                if validatePartial ch frequencies = false 
                    then 0
                else                    
                    (test (String.concat "." [str1; str2]) frequencies) controlSum (maxProjectedCount - 1) +
                    (test (String.concat "#" [str1; str2]) frequencies) controlSum maxProjectedCount
            | s -> failwith $"should be exactly 2 items. Got {s.Length} instead"

        // if result > 0 then
        //     printfn "input: %s; freq: %A; result %d" input frequencies result
        // Console.ReadLine() |> ignore
        result

let rec test' (input : string) (freq : int array) = 
    let maxProjectedCount = 
        input.ToCharArray()
        |> Array.filter(fun c -> c = '#' || c = '?')
        |> Array.length

    test input freq (freq |> Array.sum) maxProjectedCount 



let multiplyStr (input : string) (n : int) = 
    input 
    |> Array.replicate n
    |> String.concat "?"


let multiplyArr (arr : int array) (n : int) = 
    arr
    |> Array.replicate n
    |> Array.collect (fun i -> i)

async {
    let path = Path.Combine(__SOURCE_DIRECTORY__, "Day12.txt"); 
    let! lines = File.ReadAllLinesAsync(path) |> Async.AwaitTask

    //let lines = [| ".??..??...?##. 1,1,3" |]

    lines 
    |> Array.map parseLine 
    |> Array.map (fun (input, freq) -> (multiplyStr input 5, multiplyArr freq 5))
    |> Array.iter (fun (input, freq) -> (test' input freq) |> printfn "%d")
//    |> Array.map (fun (input, freq) -> (test input freq))
    // |> Array.sum
    // |> printfn "%d"

} |> Async.RunSynchronously

printfn $"elapsed: {sw.Elapsed}"