open System
open System.IO

let (|Rock|Ash|) (c : char) = 
    match c with
    | '.' -> Ash
    | '#' -> Rock
    | _ -> failwith $"unsupported character: {c}"

let hashLine(chars : char array) : int =
    chars
    |> Array.map (fun c -> 
        match c with
        | Ash -> 0
        | Rock -> 1)
    |> Array.fold (fun state element -> (state <<< 1) + element) 0

let rollUp (step : int) (arr : int array) = 
    let mirror = arr |> Array.take step |> Array.rev
    let rest = arr |> Array.skip step

    for index in 0..Math.Min(mirror.Length, rest.Length) - 1 do
        rest[index] <- rest[index] ^^^ mirror[index]
    
    rest

let isPowerOf2 (i : int) = 
    i &&& (i - 1) = 0

let getMirror (arr : int array) : int Option = 
    let mutable result : int Option = None

    for step in [1..arr.Length - 1] do
        let rolled = rollUp step arr
        let hasSmudgeOrMirror = 
            rolled 
            |> Array.truncate step 
            |> Array.forall isPowerOf2

        let hasSingleSmudge = 
            hasSmudgeOrMirror &&
            rolled
            |> Array.truncate step
            |> Array.filter (fun i -> i > 0)
            |> Array.tryExactlyOne 
            |> Option.isSome

        if hasSingleSmudge && result.IsNone then
            result <- Some step
    result

let buildHorizontalHashes (input : string array) = 
    input
    |> Array.map _.ToCharArray() 
    |> Array.map hashLine

let buildVerticalHashes (input : string array) = 
    let len = input[0].Length
    let arr = Array.zeroCreate len

    for line in input do
        [|for c in line -> c|] 
        |> Array.map (fun c -> match c with | Ash -> 0 | Rock -> 1)
        |> Array.iteri (fun idx e -> arr[idx] <- (arr[idx] <<< 1) + e)
    arr

let splitLinesIntoFields(lines : string array) : string array array =
    seq {
        let buffer = ResizeArray 0 

        for line in lines do
            if line |> String.length = 0 then 
                yield buffer.ToArray()
                buffer.Clear()
            else
                buffer.Add(line)
        
        if buffer.Count > 0 then
            yield buffer.ToArray()
    }
    |> Seq.toArray

let path = Path.Combine(__SOURCE_DIRECTORY__, "Day13.txt")
let lines = File.ReadAllLines(path)

let fields = 
    lines |> splitLinesIntoFields

let verticalHashes = fields |> Array.map buildVerticalHashes
let horizontalHashes = fields |> Array.map buildHorizontalHashes


printfn "vertical:"
let vResult = verticalHashes |> Array.choose getMirror |> Array.sum

printfn "horizontal:"
let hResult = horizontalHashes |> Array.choose getMirror |> Array.sumBy (fun i -> i * 100)

vResult + hResult