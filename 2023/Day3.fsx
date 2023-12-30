open System
open System.IO

let (|Digit|_|) (c: char) =
    if Char.IsDigit(c) then
        Some(c |> sprintf "%c" |> Int32.Parse)
    else
        None

let (|Part|_|) (c: char) =
    match c with
    | Digit _ -> None
    | '.' -> None
    | _ -> Some(c)

let getDigitsInLine (s: string) =
    s.ToCharArray()
    |> Array.indexed
    |> Array.filter (fun (_, c) ->
        match c with
        | Digit _ -> true
        | _ -> false)

let getPartsInLine (s: string) =
    s.ToCharArray()
    |> Array.indexed
    |> Array.filter (fun (_, c) ->
        match c with
        | Part _ -> true
        | _ -> false)

let isAdjacent (a: int * int) (b: int * int) =
    let (x1, y1) = a
    let (x2, y2) = b
    (Math.Abs(x1 - x2) <= 1) && (Math.Abs(y1 - y2) <= 1)

// isAdjacent (1, 1) (1, 0) |> printfn "adjacent: %b"
let getLeftMostDigit (s: string) (index: int) : int =
    let mutable current = index

    while current > 0 && s[current - 1] >= '0' && s[current - 1] <= '9' do
        current <- current - 1

    current

// getLeftMostPoint ".9.343........4" 14 |> printf "leftmost: %d"

let getNumberForPoint (s: string) (index: int) =
    let mutable current = index
    let mutable result = 0
    
    while current < s.Length && s[current] >= '0' && s[current] <= '9' do
        result <- 
            match s[current] with
            | Digit d -> result * 10 + d
            | _ -> result
        current <- current + 1
    
    result


async {
    let! lines =
        File.ReadAllLinesAsync(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Day3.txt"))
        |> Async.AwaitTask

    let linesArr = lines |> Seq.toArray

    let digits =
        linesArr
        |> Array.map getDigitsInLine
        |> Array.mapi (fun y arr -> arr |> Array.map (fun (x, _) -> (x, y)))
        |> Array.collect (fun i -> i)

    // digits |> printfn "digits: %A"

    let parts =
        linesArr
        |> Array.map getPartsInLine
        |> Array.mapi (fun y arr -> arr |> Array.map (fun (x, _) -> (x, y)))
        |> Array.collect (fun i -> i)

    parts |> printfn "parts: %A"

    let adjacentDigits =
        digits
        |> Array.filter (fun dxy -> 
            parts 
            |> Array.exists (fun pxy -> isAdjacent pxy dxy))

    //printfn "Adjacent digits: %A" adjacentDigits

    let adjacentDigitsByLine (lineNumber: int) = 
        adjacentDigits 
        |> Array.filter (fun (x, y) -> y = lineNumber)

    let result =
        linesArr 
        |> Array.mapi (fun i s -> 
            adjacentDigitsByLine i
            |> Array.map (fun (x, _) -> getLeftMostDigit s x)
            |> Array.distinct
            |> Array.map (fun ii -> getNumberForPoint s ii))
        |> 
        Array.collect (fun c -> c)

    result |> Array.sum |> printfn "result: %d" 

}
|> Async.RunSynchronously
