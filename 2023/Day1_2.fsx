open System
open System.IO

async {

    let! lines = File.ReadAllLinesAsync("Day1.txt") |> Async.AwaitTask

    let starts (s: string) (sub: string) = s.StartsWith sub
    let ends (s: string) (sub: string) = s.EndsWith sub

    let searchWords (s: string) f =
        if f s "one" then 1
        elif f s "two" then 2
        elif f s "three" then 3
        elif f s "four" then 4
        elif f s "five" then 5
        elif f s "six" then 6
        elif f s "seven" then 7
        elif f s "eight" then 8
        elif f s "nine" then 9
        else 0

    let searchDigits (s: string) f =
        if f s "1" then 1
        elif f s "2" then 2
        elif f s "3" then 3
        elif f s "4" then 4
        elif f s "5" then 5
        elif f s "6" then 6
        elif f s "7" then 7
        elif f s "8" then 8
        elif f s "9" then 9
        else 0

    let rec searchDirect (s: string) =
        let d = searchDigits s starts
        let w = searchWords s starts

        if s.Length = 0 then 0
        elif d > 0 then d
        elif w > 0 then w
        else searchDirect (s.Substring 1)

    let rec searchReverse (s: string) =
        let d = searchDigits s ends
        let w = searchWords s ends

        if s.Length = 0 then 0
        elif d > 0 then d
        elif w > 0 then w
        else searchReverse (s.Substring(0, s.Length - 1))

    lines |> Seq.sumBy (fun e -> searchDirect e * 10 + searchReverse e) |> Console.WriteLine |> ignore

}
|> Async.RunSynchronously
