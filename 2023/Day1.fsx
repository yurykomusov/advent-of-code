open System
open System.IO

async {

    let! lines = File.ReadAllLinesAsync("Day1.txt") |> Async.AwaitTask

    let calc (line : string) = 
        let lineArr = line.ToCharArray()
        let first = lineArr |> Seq.find Char.IsDigit |> string |> Int32.Parse
        let last = lineArr |> Seq.findBack Char.IsDigit |> string |> Int32.Parse
        printfn $"Reading line: first={first} last={last}"
        (first * 10) + last


    lines 
    |> Seq.sumBy calc 
    |> Console.WriteLine 
    |> ignore

}
|> Async.RunSynchronously
