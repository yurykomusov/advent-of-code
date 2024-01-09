open System.IO

type Direction =
| Top
| Right
| Bottom
| Left
    member this.vector =
        match this with
        | Top -> (-1, 0)
        | Right -> (0, 1)
        | Left -> (0, -1)
        | Bottom -> (1, 0)

    member this.opposite = 
        match this with
        | Top -> Bottom
        | Bottom -> Top
        | Left -> Right
        | Right -> Left

// naming is kinda screwed up... define input directions
let (|TopBottom|LeftRight|TopLeft|TopRight|BottomLeft|BottomRight|Ground|) (c :char) =
    match c with
    | '|' -> TopBottom (Top, Bottom)
    | '-' -> LeftRight (Left, Right)
    | 'F' -> BottomRight (Top, Left)
    | '7' -> BottomLeft (Top, Right)
    | 'J' -> TopLeft (Right, Bottom)
    | 'L' -> TopRight (Bottom, Left)
    | '.' -> Ground
    | _ -> failwith (sprintf "unexpected symbol %c" c)

let (|Start|_|) (c : char) = 
    if c = 'S' then Some Start else None

let calcDirection 
    (pipeInput1 : Direction) 
    (pipeInput2 : Direction) 
    (prev : Direction) =
    
    if prev = pipeInput1 then 
        Some pipeInput2.opposite
    elif prev = pipeInput2 then
        Some pipeInput1.opposite
    else
        None    

let nextDirection (prev : Direction) (cell : char) = 
    //printf "checking cell %c; direction: %A; " cell prev
    let result =
        match cell with
        | TopBottom (d1, d2) -> calcDirection d1 d2 prev 
        | LeftRight (d1, d2) -> calcDirection d1 d2 prev
        | TopLeft (d1, d2) -> calcDirection d1 d2 prev
        | TopRight (d1, d2) -> calcDirection d1 d2 prev
        | BottomLeft (d1, d2) -> calcDirection d1 d2 prev
        | BottomRight (d1, d2) -> calcDirection d1 d2 prev
        | Ground -> None
    
    printfn "next direction : %A" result

    result

let findStart (map2d) =
    let mutable startXY = (-1, -1)

    for x in 0..Array2D.length1 map2d - 1 do
        for y in 0..Array2D.length2 map2d - 1 do
            match map2d.[x, y] with
            | Start -> 
                startXY <- (x, y)
            | _ -> 
                startXY <- startXY
    startXY

let nextCoordinates (xy : int * int) (d : Direction) = 
    let (x, y) = xy
    let (dx, dy) = d.vector
    (x + dx, y + dy)

let findStartDirections (map2d : char array2d) (startXY : int * int) = 
    let topCellXY = nextCoordinates startXY Top
    let bottomCellXY = nextCoordinates startXY Bottom
    let leftCellXY = nextCoordinates startXY Left
    let rightCellXY = nextCoordinates startXY Right

    let nextDirectionsArr =  
        [| (topCellXY, Top); (bottomCellXY, Bottom); (rightCellXY, Right); (leftCellXY, Left); |]
        |> Array.filter (fun ((x, y), _) -> 
            (x >= 0 && x < Array2D.length1 map2d) &&
            (y >= 0 && y < Array2D.length2 map2d))
        |> Array.map (fun ((x, y), d) -> (map2d.[x, y], d))
        |> Array.map (fun (cell, d) -> (d, nextDirection d cell))
        |> Array.filter (fun (_, nextOp) -> Option.isSome nextOp)
        |> Array.map (fun (prev, _) -> prev)

    match nextDirectionsArr with
    | [| first; second |] -> (first, second)
    | _ -> failwith "something isn't right. Must be 2 available directions"

async {
    let! lines =
        File.ReadAllLinesAsync(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Day10.txt"))
        |> Async.AwaitTask
  
    let map = 
        lines
        |> Array.map _.ToCharArray()
        
    let map2d = Array2D.init 140 140 (fun x y -> map[x][y])

    let startXY = findStart map2d

    printfn "start: %A" startXY

    let startDirections = findStartDirections map2d startXY 

    let path (map : char array2d) (xy : int * int) (direction : Direction) = 
        let mutable currentXY = xy
        let mutable currentDirection = direction
        
        seq {
            while 1=1 do
                currentXY <- nextCoordinates currentXY currentDirection
                let (currentX, currentY) = currentXY
                printfn "next coordinates : %A" currentXY
                match nextDirection currentDirection map.[currentX, currentY] with
                | Some next -> (currentDirection <- next)
                | None -> failwith "impossible direction. check map"

                yield (currentDirection, (currentX, currentY), map.[currentX, currentY])
            }

    let (firstD, secondD) = startDirections
    let seq1 = path map2d startXY firstD


    

    // let seq2 = path map2d startXY secondD



    // (seq1, seq2) 
    // ||> Seq.map2 (fun (_, xy1, _) (_, xy2, _) -> (xy1, xy2))
    // |> Seq.takeWhile (fun (xy1, xy2) -> xy1 <> xy2)
    // |> Seq.length
    // |> printfn "result: %d"

    printfn "%A" startDirections
    


} |> Async.RunSynchronously