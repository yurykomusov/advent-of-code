open System.IO

//let arr = [|10;  13;  16;  21;  30;  45; |]


let rec next (arr : int array) =
    if arr |> Array.forall (fun i -> i = 0) then
        0
    else 
        let nextArr = 
            arr 
            |> Array.pairwise
            |> Array.map (fun (a, b) -> b - a)
        
        Array.last arr + (next nextArr)


let rec prev (arr : int array) =
    if arr |> Array.forall (fun i -> i = 0) then
        0
    else 
        let nextArr = 
            arr 
            |> Array.pairwise
            |> Array.map (fun (a, b) -> b - a)
        
        Array.head arr - (prev nextArr)

async {
    let! lines =
        File.ReadAllLinesAsync(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Day9.txt"))
        |> Async.AwaitTask

    lines 
        |> Array.map (fun line -> 
            line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
        |> Array.map prev
        |> Array.sum
        |> printfn "%d"

} |> Async.RunSynchronously