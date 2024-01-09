open System.Text.RegularExpressions
open System.IO
open System

type P2PMap = Map<string, (string * string)>
type DistanceMap = System.Collections.Generic.Dictionary<(string * int), (int * string)>

let sequence (pattern: char array) =
    seq {
        while 1=1 do
            yield! pattern
    }
    
let (|ParsedExpression|_|) (pattern: string) (input: string) =
    let r = new Regex(pattern)
    let matches = r.Matches(input)

    if matches.Count > 0 then
        Some([| for x in matches -> x.Value |])
    else
        None


let (|Left|Right|) (c: char) =
    if c = 'L' then Left
    elif c = 'R' then Right
    else failwith "Incorrect direction character: "

[<return: Struct>]
let (|Start|_|) (s: string) =
    if (s[2] = 'A') then ValueSome Start else ValueNone

[<return: Struct>]
let (|End|_|) (s: string) = if (s[2] = 'Z') then ValueSome End else ValueNone

let parseLine (str: string) =
    match str with
    | ParsedExpression "[A-Z0-9]{3}" matches -> (matches[0], (matches[1], matches[2]))
    | _ -> failwith (sprintf "no match %s" str)

let inline goLeft (from: string) (map: P2PMap) =
    let (left, _) = map[from]
    left

let inline goRight (from: string) (map: P2PMap) =
    let (_, right) = map[from]
    right

let inline getNext (map: P2PMap) (from: string) (direction: char) =
    //printfn "calculating next point: origin: %s - direction %c" from direction
    match direction with
    | Left -> (goLeft from map)
    | Right -> (goRight from map)

let inline isEnd (point : string) = match point with | End -> true | _ -> false
let inline isStart (point : string) = match point with | Start -> true | _ -> false

async {
    let! lines =
        File.ReadAllLinesAsync(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Day8.txt"))
        |> Async.AwaitTask

    let directions = (Array.head lines).ToCharArray()
    
    let map = 
        lines 
        |> Array.skip 2 
        |> Array.map parseLine 
        |> Map 

    let startPoints =
        [| for i in map.Keys -> i |]
        |> Array.filter isStart

    let getNext' = getNext map

    let sw = new System.Diagnostics.Stopwatch()

    sw.Start()

//    let startPoint = startPoints[0]

    // let directionSequence = sequence directions
        
    // let firstZPoint = 
    //     directionSequence 
    //     |> Seq.scan (fun p d -> getNext' p d) startPoint
    //     |> Seq.skipWhile (fun p -> not (match p with | End -> true | _ -> false))
    //     |> Seq.take 1 

    // printfn "z-point: %A" firstZPoint



    let zSequence (point : string) = 
        seq {
            let mutable cnt = 0L
            let mutable current = point

            for d in (sequence directions) do
                cnt <- cnt + 1L
                current <- getNext' current d
                
                if isEnd current then
                    yield (current, cnt)
                    cnt <- 0L
        }


    // let zs = 
    //     startPoints
    //     |> Array.map (fun sp -> zSequence sp)
    //     |> Array.take 3

    // let mutable result = 0L
    // let mutable matchCount = 0
    // let mutable currentIndex = 0

    // while matchCount < startPoints.Length - 1 do
    //     let possible = 
    //         zs[currentIndex]
    //         |> Seq.scan (fun counter (_, dist) -> counter + dist) 0L 
    //         |> Seq.skip 1
    //         |> Seq.skipWhile (fun dist -> dist < result) 
    //         |> Seq.head
        
    //     //printfn "current result : %d ; possible %d; match: %d" result possible matchCount

    //     if possible = result then
    //         matchCount <- matchCount + 1
    //         currentIndex <- currentIndex + 1
    //         printfn "match found - %d result - %d" matchCount result
    //     else
    //         result <- possible
    //         matchCount <- 0
    //         currentIndex <- 
    //             match currentIndex with
    //             | 0 -> 1
    //             | _ -> 0

    // printfn "%d" result

    startPoints 
    |> Array.map zSequence
    |> Array.map (fun s -> s |> Seq.head)
    |> Array.map (fun (_, dist) -> dist)
    |> printfn "%A"


    // zSequence startPoints[5] 
    // |> Seq.truncate 2 
    // // |> Seq.scan (fun state (_, dist) -> state + dist) 0L
    // |> Seq.iter (printfn "%A")
    // // |> Seq.iter (fun (p, dist) -> printfn "point : %s step : %d" p (dist % 256))    


    printfn "elapsed %A" sw.Elapsed

}
|> Async.RunSynchronously
