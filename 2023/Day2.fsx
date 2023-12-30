open System
open System.IO

let parseGameId (s: string) =
    s.Substring(5) |> int

let subColor (s: string) (color: string) =
    let result =
        s.Split(',', StringSplitOptions.TrimEntries)
        |> Array.tryFind (fun s -> s.Contains(color))

    match result with
    | Some s -> s.Substring(0, s.Length - color.Length) |> int
    | None -> 0

let subset (s: string) =
    (subColor s "red", subColor s "green", subColor s "blue")


let split (s: string) = s.Split(";", StringSplitOptions.TrimEntries)
let subsets (s: string) = (s |> split) |> Array.map subset
let parse (s: string) = 
    let chunks = s.Split(":", StringSplitOptions.TrimEntries)
    (parseGameId chunks[0], subsets chunks[1])

// let (id, sets) = parse "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

//printfn "%d %A" id sets

async {
    let! lines = File.ReadAllLinesAsync("Day2.txt") |> Async.AwaitTask

    lines 
    |> Seq.map parse
    |> Seq.filter (fun (_, sets) -> sets |> Array.forall (fun (r, g, b) -> r <= 12 && g <= 13 && b <= 14)) 
    |> Seq.sumBy (fun (id, _) -> id)
    |> printfn "%d"
    |> ignore 

} |> Async.RunSynchronously