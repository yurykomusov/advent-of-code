#r "nuget: FSharp.Control.AsyncSeq"

open System
open System.Text.RegularExpressions
open System.IO
open FSharp.Control

let (|ParsedExpression|_|) (pattern: string) (input: string) =
    let r = new Regex(pattern)
    let matches = r.Match(input)

    if matches.Success then
        Some([| for x in matches.Groups[1].Captures -> x.Value |])
    else
        None

type Rng =
    { Start: Int64
      Count: Int64 }

    member x.End() = x.Start + x.Count - 1L

    member this.Split(point: Int64) =
        match point with
        | p when p > this.Start && p < this.End() ->
            [| { Start = this.Start
                 Count = p - this.Start }
               { Start = p
                 Count = this.End() - p + 1L } |]
        | _ -> [| this |]

    member this.SplitLeft(point: Int64) =
        match point with
        | p when p > this.Start && p < this.End() ->
            [| { Start = this.Start
                 Count = p - this.Start - 1L }
               { Start = p
                 Count = this.End() - p + 2L } |]
        | _ -> [| this |]



type SourceDestMapType =
    | SeedToSoil
    | SoilToFetrilizer
    | FertilizerToWater
    | WaterToLight
    | LightToTemperature
    | TemperatureToHumidity
    | HumidityToLocation

type SourceDestMap =
    { Source: Int64
      Dest: Int64
      Range: Int64 }

    member this.SourceRange =
        { Start = this.Source
          Count = this.Range }

    member this.DestRange =
        { Start = this.Dest
          Count = this.Range }

    member this.apply(input: Int64) : (Int64 * bool) =
        let lower = this.Source
        let upper = this.Source + this.Range - 1L

        // printf $"input: {state} ->"

        if input >= lower && input <= upper then
            let result = input + this.Dest - this.Source
            //   printfn $" output: {result}"
            (result, true)
        else
            //    printfn $" output: {state}"
            (input, false)

type LineType =
    | SeedsList of Int64 array
    | Mapp of SourceDestMap
    | Header of SourceDestMapType
    | Empty

let parseLine (line: string) =
    match line with
    | ParsedExpression "seeds: (\\d+\\s?)+" arr -> SeedsList(arr |> Array.map Int64.Parse)
    | "seed-to-soil map:" -> Header SeedToSoil
    | "soil-to-fertilizer map:" -> Header SoilToFetrilizer
    | "fertilizer-to-water map:" -> Header FertilizerToWater
    | "water-to-light map:" -> Header WaterToLight
    | "light-to-temperature map:" -> Header LightToTemperature
    | "temperature-to-humidity map:" -> Header TemperatureToHumidity
    | "humidity-to-location map:" -> Header HumidityToLocation
    | ParsedExpression "(\\d+\\s?)+" arr ->
        let numbers = arr |> Array.map Int64.Parse

        match numbers with
        | [| dest; src; range |] ->
            Mapp
                { Source = src
                  Dest = dest
                  Range = range }
        | _ -> failwith "invald map size"
    | "" -> Empty
    | s -> failwith $"incomprehensive input: {s}"

let generateRanges (input: Int64 array) =
    input
    |> Array.chunkBySize 2
    |> Array.map (fun pair ->
        match pair with
        | [| a; b |] -> { Start = a; Count = b }
        | _ -> { Start = 0L; Count = 0L })

let squash (lines: LineType array) =
    let startHeader = Header SeedToSoil

    let (linesWithHeaders, _) =
        lines
        |> Array.skip 1
        |> Array.mapFold
            (fun header line ->
                match line with
                | SeedsList s -> failwith ("seeds list should be skipped. Got " + sprintf "%A" s)
                | Header _ -> ((line, line), line) // switch header
                | Mapp _ -> ((line, header), header)
                | Empty -> ((line, header), header))
            startHeader

    let linesByHeader =
        linesWithHeaders
        |> Array.filter (fun (item, _) ->
            match item with
            | Mapp _ -> true
            | _ -> false)
        |> Array.groupBy (fun (_, header) -> header)
        |> Array.map (fun (key, pairs) -> (key, pairs |> Array.map (fun (item, _) -> item)))
        |> Array.map (fun (header, maps) ->
            let typedmaps =
                maps
                |> Array.map (fun m ->
                    match m with
                    | Mapp mt -> mt
                    | _ -> failwith "item expected to be a map")

            (header, typedmaps))

    linesByHeader

let handleRange (maps: SourceDestMap[]) (r: Rng) =
    let isPointInsideRange (r: Rng) (p: Int64) = p > r.Start && p < (r.End())

    printfn "handling range: %A; maps: %A" r maps

    let intersectPoints =
        maps
        |> Array.map (fun m -> [| m.SourceRange.Start; m.SourceRange.End() + 1L |])
        |> Array.collect (fun i -> i)
        |> Array.filter (isPointInsideRange r)

    let intersectPointsWitNoNeighbours =
        intersectPoints
        |> Array.sort
        |> Array.indexed
        |> Array.filter (fun (index, point) ->
            if index = 0 then
                true
            elif index > 0 && point - intersectPoints[index - 1] = 1 then
                false // filter out neighbour points. no split required here
            else
                false)
        |> Array.map (fun (_, v) -> v)

    printfn "found intersect point: %A" intersectPoints
    printfn "after filtering neighbour: %A" intersectPointsWitNoNeighbours

    let (other, last) =
        intersectPointsWitNoNeighbours
        |> Array.mapFold
            (fun (r: Rng) p ->
                match r.Split p with
                | [| left; right |] -> (left, right)
                | _ -> failwith "unexpected amount of elements after split")
            r

    let applyMaps (maps': SourceDestMap array) (r': Rng) =
        let sourceStart = r'.Start
        let sourceEnd = r'.End()

        let mutable destStart = 0L
        let mutable destEnd = 0L

        let mutable skip = false

        for m' in maps' do
            if not skip then
                let (destStart', changed) = m'.apply sourceStart
                let (destEnd', _) = m'.apply sourceEnd
                destStart <- destStart'
                destEnd <- destEnd'

                if changed then
                    skip <- true

        { Start = destStart
          Count = destEnd - destStart + 1L }

    let splitRanges =
        seq {
            yield! other
            yield last
        }

    printfn "range is split into: %A" splitRanges

    let appliedRanges = splitRanges |> Seq.map (applyMaps maps) |> Seq.toArray

    printfn "output ranges: %A" appliedRanges

    appliedRanges

let lines = File.ReadLinesAsync(Path.Combine(__SOURCE_DIRECTORY__, "Day5.txt"))


// let srcdestmap = { Source = 1L; Range = 10L; Dest = 100L }
// let input = { Start = 5L; Count = 15L}

// handleRange [| srcdestmap |] input |> printfn "test output: %A"


// let input = { Start = 81L; Count = 14L }
// let srcdestmap = { Source = 25L; Dest = 18L; Range = 70L }

// handleRange [| srcdestmap |] input |> ignore


let typedLines =
    lines
    |> AsyncSeq.ofAsyncEnum
    |> AsyncSeq.map parseLine
    |> AsyncSeq.toArraySynchronously

let seedsLine = typedLines[0]

let squashedMaps = typedLines |> squash

let seeds =
    match seedsLine with
    | SeedsList seeds -> seeds |> generateRanges
    | _ -> failwith "incorrect seed list"

let result =
    squashedMaps
    |> Array.fold (fun seeds' (_, maps) -> 
        printfn "===== next map ====="
        seeds' 
        |> Array.map (handleRange maps) |> Array.collect (fun i -> i)) seeds

printfn "some result: %A" result


result
|> Array.map (fun r -> [| r.Start; r.End() |])
|> Array.collect (fun r -> r)
|> Array.min
|> printfn "min %d"
