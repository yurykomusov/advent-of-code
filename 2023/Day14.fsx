let (|Cubic|Rounded|Empty|) (c : char) = 
    match c with
    | '#' -> Cubic
    | 'O' -> Rounded
    | '.' -> Empty
    | c -> failwith $"invalid cell type: {c}"

type Direction = 
    | North
    | South
    | West
    | East
    member this.Vector : int * int = 
        match this with
        | North -> (0, -1)
        | South -> (0, 1)
        | West -> (-1, 0)
        | East -> (1, 0)

let print2d (f : char array2d) : unit = 
    let len1 = f |> Array2D.length1
    let len2 = f |> Array2D.length2

    for y in 0 .. (len1 - 1) do
        printfn ""
        for x in 0 .. (len2 - 1) do
            printf "%c" f.[x,y]
    

let field = Array2D.init 10 10 (fun x y -> if y = 9 then '#' else  '.')

let applyGravityToPoint (f : char array2d) ((x, y) : int * int) ((gx, gy) : int * int) = 
    
    // only Os are moving 
    if f.[x, y] = 'O' then 
        ignore 1
        // let current is on the edge skip    
    elif x + gx < 0 0 || y + gy < 0 then
        f
    else
        let next = f.[x + gx, y + gy]
        

print2d field




// let tick (current : int * int) (tiltDirection : Direction) (field : char array2d) = 
