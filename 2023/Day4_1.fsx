open System
open System.IO
open System.Text.RegularExpressions

let (|ParsedExpression|_|) (pattern: string) (input: string) =
    let r = new Regex(pattern)
    let m = r.Matches(input)

    if m.Count > 0 then
        Some([| for x in m -> x.Groups[0].Value |])
    else
        None

let handleLine (s: string) =
    let start = s.IndexOf(":")
    let ss = s.Substring(start)

    let matchCount =
        match ss with
        | ParsedExpression "\d{1,3}" array ->
            array
            |> Array.countBy (fun i -> i)
            |> Array.filter (fun (_, count) -> count > 1)
            |> Array.length
        | _ -> 0

    matchCount

async {
    let! lines =
        File.ReadAllLinesAsync(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Day4.txt"))
        |> Async.AwaitTask


    let winsArr = lines |> Seq.toArray |> Array.map handleLine
    let cards = Array.replicate lines.Length 1

    let calcNextCopies (state: int array) ((index, matchCount): int * int) =
        let currentCardsCount = state[index]

        printfn "iteration %d. current state: %A. match count: %d" index state matchCount

        state
        |> Array.mapi (fun idx cardCount ->
            if idx > index && idx <= index + matchCount then
                cardCount + currentCardsCount
            else
                cardCount)

    winsArr 
    |> Array.indexed 
    |> Array.fold calcNextCopies cards 
    |> Array.sum
    |> printfn "%d"




}
|> Async.RunSynchronously




// [0] 0
// [1] w2
// [2] w3 w[1]
// [3] w1 w[1] w[2]
// [4] w0 w[2]
// [5] w0 w[2]
