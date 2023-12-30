// let time = 7
// let record = 9

let round (time: int64) (record: int64) = 
    [0L..time] 
    |> List.fold (
        fun state time_hold ->
            let distance = time_hold * (time - time_hold)
            // printf "calculated distance for t=%d is %d" time_hold distance 
            if distance > record then
                state + 1L
            else 
                state)
        0L

//let input = [| (71530, 940200) |]
let input = [| (44899691L, 277_1136_1890_1768L); |]

input
|> Array.map (fun (time, record) -> round time record) 
// |> Array.fold (fun a b -> a * b) 1
|> printfn "%A"
