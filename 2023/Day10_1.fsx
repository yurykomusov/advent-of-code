open System.IO

let (LEN1, LEN2) = (140, 140)

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

    member this.clockwise90 =
        match this with 
        | Top -> Right
        | Right -> Bottom
        | Bottom -> Left
        | Left -> Top

    member this.anticlockwise90 =
        match this with 
        | Top -> Left
        | Right -> Top
        | Bottom -> Right
        | Left -> Bottom



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
    
    //printfn "next direction : %A" result

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
        File.ReadAllLinesAsync(Path.Combine(__SOURCE_DIRECTORY__, "Day10.txt"))
        |> Async.AwaitTask
  
    let map = 
        lines
        |> Array.map _.ToCharArray()
        
    let map2d = Array2D.init LEN1 LEN2 (fun x y -> map[x][y])

    let startXY = findStart map2d

    printfn "start: %A" startXY

    let startDirections = findStartDirections map2d startXY 

    let pathSeq (map : char array2d) (xy : int * int) (direction : Direction) = 
        let mutable currentXY = xy
        let mutable currentDirection = direction
        let mutable startReached = false

        seq {
            while not startReached do
                currentXY <- nextCoordinates currentXY currentDirection
                let (currentX, currentY) = currentXY
                //printfn "next coordinates : %A" currentXY
                
                startReached <-
                    match map.[currentX, currentY] with
                    | Start -> true
                    | _ -> false

                if not startReached then
                    match nextDirection currentDirection map.[currentX, currentY] with
                    | Some next -> (currentDirection <- next)
                    | None -> failwith "impossible direction. check map"

                    yield (currentDirection, (currentX, currentY), map.[currentX, currentY])
            }

    let (firstD, secondD) = startDirections

    let path = pathSeq map2d startXY firstD |> Seq.toArray
    let pathXYs = path |> Array.map (fun (_, xy, _) -> xy)

    let getInsidesForPoint (d : Direction) (xy : int * int) = 
            let mutable reachedPath = false
            let mutable currentXY = xy

            seq {
                while not reachedPath do
                    currentXY <- d |> nextCoordinates currentXY
                    if pathXYs |> Array.exists (fun i -> currentXY = i) then
                        reachedPath <- true
                    else    
                        yield currentXY
            }

    let path2 = pathSeq map2d startXY secondD |> Seq.toArray
    let pathXY2s = path2 |> Array.map (fun (_, xy, _) -> xy)

    let insides =  
        path
        |> Array.map (fun (d, xy, _) -> getInsidesForPoint d.clockwise90 xy)
        |> Array.collect (fun i -> i |> Seq.toArray)
        |> Array.filter (fun xy -> pathXYs |> Array.exists (fun i -> i = xy) |> not)
        |> Array.distinct

    let insides2 =  
        path2
        |> Array.map (fun (d, xy, _) -> getInsidesForPoint d.anticlockwise90 xy)
        |> Array.collect (fun i -> i |> Seq.toArray)
        |> Array.filter (fun xy -> pathXY2s |> Array.exists (fun i -> i = xy) |> not)
        |> Array.distinct


    let insidesAll =     
        [| insides; insides2 |]
        |> Array.concat
        |> Array.distinct

    insidesAll
    |> Array.iter (fun (x, y) -> 
        map2d.[x, y] <- 'I')
    
    // Note there's a bug that includes S point into insides. So add 1 for correct answer
    printfn "result: %d" insidesAll.Length

    // path2
    // |> Array.iter (fun (d, (x, y), _) -> 
    //     map2d.[x, y] <- 
    //         match d with
    //         | Top -> '^'
    //         | Bottom -> 'v'
    //         | Left -> '<'
    //         | Right -> '>')

    // map2d 
    // |> Array2D.iteri (fun x y item ->
    //     if item = 'I' then
    //         System.Console.ForegroundColor <- System.ConsoleColor.Cyan
    //     elif
    //         item = 'v' || item = '^' || item = '<' || item = '>' then
    //         System.Console.ForegroundColor <- System.ConsoleColor.Yellow
    //     else
    //         System.Console.ResetColor()
        
    //     if y % LEN2 = (LEN2 - 1) then
    //         System.Console.WriteLine("{0}", item)
    //     else
    //         System.Console.Write("{0}", item))

} |> Async.RunSynchronously