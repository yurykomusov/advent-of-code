open System
open System.IO

let (|Star|Void|) (c: char) = if c = '#' then Star else Void

let getVerticalLines (arr2d: char array2d) =
    let len1 = Array2D.length1 arr2d
    let len2 = Array2D.length2 arr2d
    
    [| for i in 0 .. len1 - 1 ->
           [| for j in 0 .. len2 - 1 -> arr2d[j, i] |] |]

let getHorizontalLines (arr2d: char array2d) =
    let len1 = Array2D.length1 arr2d
    let len2 = Array2D.length2 arr2d
    
    [| for i in 0 .. len2 - 1 ->
           [| for j in 0 .. len2 - 1 -> arr2d[i, j] |] |]

let checkVoidness (input: char array array) = 
    input 
    |> Array.map (
        fun line -> 
            line
            |> Array.forall (fun c -> 
                match c with 
                | Void -> true 
                | _ -> false))
    |> Array.indexed
    |> Array.filter (fun (_, item) -> item)
    |> Array.map (fun (index, _) -> index)
    |> Array.sort

let calcDistance
    ((p1x, p1y): int * int) 
    ((p2x, p2y): int * int) 
    (vertvoids: int array) 
    (horvoids: int array) =

    // taking into account that points might be not sorted
    let vertical = 
        vertvoids 
        |> Array.filter (fun vy -> 
            (p1y > vy && p2y < vy) || 
            (p2y > vy && p1y < vy))
        |> Array.length

    let horizontalSkew = 
        horvoids 
        |> Array.filter (fun vx -> 
            (p1x > vx && p2x < vx) || 
            (p2x > vx && p1x < vx))
        |> Array.length

    int64 (Math.Abs(p1x - p2x)) + 
    int64 (Math.Abs(p1y - p2y)) +
    int64 (horizontalSkew * (1_000_000 - 1)) +
    int64 (vertical * (1_000_000 - 1))


async {
    let path = Path.Combine(__SOURCE_DIRECTORY__, "Day11.txt"); 
    let! lines = File.ReadAllLinesAsync(path) |> Async.AwaitTask

    let lenX = lines[0].Length
    let lenY = lines.Length


    let field = Array2D.init lenX lenY (fun y x -> lines[y][x])


    let stars =
        Seq.cache (
            seq {
                for x in 0 .. Array2D.length1 field - 1 do
                    for y in 0 .. Array2D.length2 field - 1 do
                        match field.[x, y] with
                        | Star -> yield (x, y)
                        | _ -> ()
            }
        )

    let starPairs =
        stars
        |> Seq.mapi (fun i s -> stars |> Seq.skip (i + 1) |> Seq.map (fun ss -> (s, ss)))
        |> Seq.collect (fun i -> i)

    let verticalVoids = 
        field
        |> getVerticalLines 
        |> checkVoidness 

    let horizontalVoids = 
        field 
        |> getHorizontalLines 
        |> checkVoidness 

    let distances = 
        starPairs 
        |> Seq.map (fun (p1, p2) -> calcDistance p1 p2 verticalVoids horizontalVoids)
        |> Seq.toArray

    let mutable result = 0L
    distances |> Array.iter (fun i -> result <- result + int64 i)

    printfn "%d" result
} |> Async.RunSynchronously