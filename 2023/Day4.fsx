open System
open System.IO
open System.Text.RegularExpressions

let (|ParsedExpression|_|) (pattern: string) (input: string) = 
    let r = new Regex(pattern)
    let m = r.Matches(input)

    if m.Count > 0 then
        Some ([|for x in m -> x.Groups[0].Value|])
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

    let power = matchCount - 1

    2. ** power |> int


async {
    let! lines =
        File.ReadAllLinesAsync(Path.Combine(__SOURCE_DIRECTORY__, "Day4.txt"))
        |> Async.AwaitTask
        

    let result = lines |> Seq.toArray |> Array.sumBy handleLine 

    result |> printfn "result: %d"

} |> Async.RunSynchronously