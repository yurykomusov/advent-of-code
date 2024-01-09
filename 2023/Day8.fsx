open System.Text.RegularExpressions
open System.IO

let sequence (pattern : string) =
    seq {
        let mutable i = 0
        while 1 = 1 do
            yield pattern[i]
            i <- (i + 1) % pattern.Length
    }

let (|ParsedExpression|_|) (pattern: string) (input: string) =
    let r = new Regex(pattern)
    let matches = r.Matches(input)

    if matches.Count > 0 then
        Some([| for x in matches -> x.Value |])
    else
        None

let (|Left|Right|) (c: char) =
    if c = 'L' then
        Left
    elif c = 'R' then
        Right
    else
        failwith "Incorrect direction character: "


let parseLine (str: string) =  
    match str with
    | ParsedExpression "[A-Z]{3}" matches -> (matches[0], (matches[1], matches[2]))
    | _ -> failwith (sprintf "no match %s" str) 

let goLeft (from: string) (map: Map<string, (string * string)>) = 
    let (left, _) = map[from]
    left

let goRight (from: string) (map: Map<string, (string * string)>) = 
    let (_, right) = map[from]
    right

let getNext (from: string) (direction: char) (map: Map<string, (string * string)>) =
    match direction with
    | Left -> (goLeft from map)
    | Right -> (goRight from map)

async {
    let! lines = File.ReadAllLinesAsync("Day8.txt") |> Async.AwaitTask

    let directions = 
        lines 
        |> Array.head
        |> sequence 

    let map = 
        lines 
        |> Array.skip 1
        |> Array.map parseLine
        |> Map

    let getNext' (from : string) (direction: char) = getNext from direction map

    directions
    |> Seq.scan getNext' "AAA" 
    |> Seq.takeWhile (fun i -> i <> "ZZZ")
    |> Seq.length
    |> printfn "Amound of steps :%d"

} |> Async.RunSynchronously