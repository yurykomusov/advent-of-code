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

let calcFreq (input : char array) = 
    input
    |> Array.mapi (fun index element -> (index, element))
    |> Array.filter (fun (_, element) -> element = '#')
    |> Array.map (fun (index, _) -> index)
    |> Array.mapi (fun index1 index2 -> index1 - index2)
    |> Array.countBy (fun i -> i)
    |> Array.map (fun (_, count) -> count)

let testExact (input : char array) (freqExpected : int array) : int = 
    let chunks = calcFreq input
    
    if Array.compareWith (fun a b -> a - b) chunks freqExpected = 0 then
        1
    else 
        0
let validatePartial2 (sub : char array) (currentFreq : int array) (freq : int array) = 
    let skipLast = sub |> Array.last = '#'
    let takeCount = (currentFreq |> Array.length) - (if skipLast then 1 else 0) 

    if currentFreq.Length > freq.Length then
        false
    else
        currentFreq
            |> Array.take takeCount
            |> Array.indexed
            |> Array.forall (fun (index, element) -> 
                match index with
                | _ -> index < freq.Length && element = freq[index])

    // "# 2,1,2,2 - true
    // "#." - false

let validatePartial (chunks : int array) (freq : int array) =
    let actualLen = chunks.Length
    let freqLen = freq.Length

    if chunks.Length > freqLen then
        false
    else
        chunks
        |> Array.indexed
        |> Array.forall (fun (index, element) -> 
            match index with
            | s when s < actualLen - 1 -> element = freq[index]
            | _ -> index < freqLen && element <= freq[index])

let getHints (arr : char array) (freq : int array) = 
    let freqSum = freq |> Array.sum
    let arrByType = arr |> Array.countBy (fun i -> i)

    let definitelyBroken = 
        arrByType 
        |> Array.tryFind (fun (key, _) -> key = '#') 
        |> Option.bind (fun (_, v) -> Some v)
        |> Option.defaultValue 0

    // let definitelyOperational = 
    //     arrByType 
    //     |> Array.tryFind (fun (key, _) -> key = '.') 
    //     |> Option.bind (fun (_, v) -> Some v)
    //     |> Option.defaultValue 0

    let unknown = 
        arrByType 
        |> Array.tryFind (fun (key, _) -> key = '?')
        |> Option.bind (fun (_, v) -> Some v)
        |> Option.defaultValue 0
    
    let unknownBroken = freqSum - definitelyBroken
    let unknownOperational = unknown - unknownBroken 

    (unknownOperational, unknownBroken)

let toStr (input : char array) = 
    input
    |> Array.fold (fun str c -> str + c.ToString()) ""

let rec test 
    (input : char array) 
    (expectedFreq : int array) 
    (hints : int * int) : int =         
    //printfn "input: %s hints: %A" (input |> toStr) hints
    // Console.ReadLine() |> ignore
    let (operational, broken) = hints

    match (input, expectedFreq) with
    | ([|'#'; '#';'.'; '?'; '?'|], [|2; 2|]) -> 1;
    | ([|'#'|], [|1|]) -> 1
    | ([|'#'|], _) -> 0
    | ([|'#'; '#'|], [|2|]) -> 1
    | ([|'#'; '#'|], _) -> 0
    | ([|'.'|], [||]) -> 1
    | ([|'.'|], _) -> 0
    | ([|'?'|], [|1|]) -> 1
    | ([|'?'|], _) -> 0
    | _ ->
        let result = 
            match input |> Array.tryFindIndex (fun c -> c = '?') with
            | Some 0 -> 
                let copy1 = Array.copy input
                let copy2 = Array.copy input

                copy1[0] <- '.'
                copy2[0] <- '#'

                let mutable result = 0

                if operational > 0 then
                    result <- test copy1 expectedFreq (operational - 1, broken)
                if broken > 0 then
                    result <- result + test copy2 expectedFreq (operational, broken - 1)
                result
            | Some n -> 
                let sub = input |> Array.take n
                let currentFreq = calcFreq sub
                if validatePartial2 sub currentFreq expectedFreq = false
                    then 0
                elif currentFreq |> Array.isEmpty then
                    let copy1 = Array.copy input
                    let copy2 = Array.copy input
                    
                    copy1[n] <- '.'
                    copy2[n] <- '#'

                    let mutable result = 0

                    if operational > 0 then
                        result <- test copy1 expectedFreq (operational - 1, broken)
                    if broken > 0 then
                        result <- result + test copy2 expectedFreq (operational, broken - 1)
                    result
                else
                    //printfn "current freq: %A" currentFreq
                    let expectedFreq' = expectedFreq |> Array.skip (currentFreq.Length - 1)
                    
                    let mutable remaining = 
                        currentFreq 
                        |> Array.take (currentFreq.Length - 1) 
                        |> Array.sum

                    let input' = input |> Array.skipWhile (fun i -> 
                        if i = '#' then 
                            remaining <- remaining - 1
                        remaining >= 0)

                    let (operational2, broken2) = getHints input' expectedFreq'

                    let qIndex = input' |> Array.findIndex (fun i -> i = '?')

                    if operational2 < 0 || broken2 < 0 then
                        0
                    else
                        match (operational2 > 0, broken2 > 0) with
                        | (true, true) ->
                            let copy1 = Array.copy input'
                            let copy2 = Array.copy input'
                            copy1[qIndex] <- '.'
                            copy2[qIndex] <- '#'
                            test copy1 expectedFreq' (operational2 - 1, broken2) +
                            test copy2 expectedFreq' (operational2, broken2 - 1)
                        | (true, false) -> 
                            let copy1 = Array.copy input'
                            copy1[qIndex] <- '.'
                            test copy1 expectedFreq' (operational2 - 1, broken2)
                        | (false, true) ->
                            let copy2 = Array.copy input'
                            copy2[qIndex] <- '#'
                            test copy2 expectedFreq' (operational2, broken2 - 1)   
                        | (false, false) -> failwith "should be impossible"

            | None -> testExact input expectedFreq
                
        if result > 0 then
            printfn "input: %A; freq: %A; result %d" (input |> toStr)  expectedFreq result
        //     // Console.ReadLine() |> ignore
        result

let rec test' (input : string) (freq : int array) = 
    let arr = input.ToCharArray() 

    let hints = getHints arr freq

    test arr freq hints

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

// let input = [|'#'; '.'; ','; '.'|]
// let currentFreq = calcFreq input
// validatePartial2 input currentFreq [|1; 1;|] |> printfn "%b"