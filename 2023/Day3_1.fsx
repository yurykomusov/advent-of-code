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

let (|Gear|_|) (c: char) = 
    match c with
    | Part p when p = '*' -> Some(p)
    | _ -> None

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

    let isGear (c: char) = 
        match c with
        | Gear _ -> true
        | _ -> false 

    let gears =
        linesArr
        |> Array.map (getPartsInLine >> Array.filter (fun (_, p) -> isGear p)) 
        |> Array.mapi (fun y arr -> arr |> Array.map (fun (x, _) -> (x, y)))
        |> Array.collect (fun i -> i)

    gears |> printfn "gears: %A"

    // let adjacentDigits =
    //     digits
    //     |> Array.filter (fun dxy -> 
    //         gears 
    //         |> Array.exists (fun pxy -> isAdjacent pxy dxy))

    // let gearsWith2numbers = 
    //     gears 
    //     |> Array.filter (fun gxy -> 
    //         digits 
    //         |> Array.filter (fun dxy -> 
    //             isAdjacent gxy dxy) 
    //             |> Array.length = 2)
    
    // gearsWith2numbers |> printfn "gears gears: %A"

    let result = 
        digits
        |> Array.groupBy (fun dxy -> 
            gears 
            |> Array.tryFind (fun pxy -> isAdjacent pxy dxy))
        |> Array.map (fun (_, neighbors) -> 
            neighbors 
            |> Array.map (fun (x, y) -> (getLeftMostDigit linesArr[y] x, y))
            |> Array.distinct
            |> Array.map (fun (x, y) -> getNumberForPoint linesArr[y] x))

    result 
    |> Array.filter (fun i -> i.Length = 2)
    |> Array.map (fun i -> i[0] * i[1])
    |> Array.sum
    |> printfn "result: %d"

}
|> Async.RunSynchronously
