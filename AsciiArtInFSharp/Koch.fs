module Koch

open Microsoft.FSharp.Collections
open AsciiDraw

// We assume that we always draw clockwise
// The snowflakeside is the direction in which there has to be constructed a new wedge
type snowflakeside = Up | UpRight | DownRight | Down | DownLeft | UpLeft
// snowflakeside, start, length
type edge = snowflakeside * point * int

let drawEdge grid (side, start, length) =
    let dir =
        match side with
        | Up -> East
        | UpRight -> SouthEast
        | DownRight -> SouthWest
        | Down -> West
        | DownLeft -> NorthWest
        | UpLeft -> NorthEast
    drawLine grid (dir, start, length) |> ignore

// (x,y) is the leftmost point
let trianglePointUp (x,y) length =
    [(Down, (x+length*2, y), length*2); (UpLeft, (x,y), length); (UpRight, (x+length, y-length), length)]
// (x,y) is the leftmost point
let trianglePointDown (x,y) length =
    [(Up, (x+length*2, y), length*2); (DownLeft, (x,y), length); (DownRight, (x+length, y+length), length)]

// TODO: add erasing lines
let processEdge grid edge =
    drawEdge grid edge
    let (side, (x, y), length) = edge
    let third = length / 3
    let sixth = third / 2
    if sixth > 0 then
        // TODO: abstract over the difference in horizontal and vertical length
        // TODO: abstract over the orientation
        let result = 
            match side with
            | Up -> 
                trianglePointUp (x + third, y) sixth
            | DownRight ->
                trianglePointUp (x - third * 2, y + third * 2) third
            | DownLeft ->
                trianglePointUp (x - third * 3, y - third) third
            | _ -> []
         // sort result s.t. Up and Down are drawn first
        result |> List.sortBy (fun (dir, _, _) -> if dir = Up || dir = Down then 0 else 1)
    else []

let rec processEdges grid edges =
    match edges with
    | edge::rest -> processEdges grid (rest @ processEdge grid edge)
    | [] -> ()

let run : char[,] =
    let grid = Array2D.init<char> 80 30 (fun x y -> ' ')

    let edges = [(Up, (0,10), 18);(DownRight, (18,10), 9);(DownLeft, (9, 19), 9)]
    processEdges grid edges |> ignore

    grid

