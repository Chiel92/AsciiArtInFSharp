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

let processEdge grid edge =
    drawEdge grid edge
    let (side, (x, y), length) = edge
    let third = length / 3
    let sixth = third / 2
    if sixth > 0 then
        match side with
        | Up -> 
            [(Up, (x, y), third); (UpLeft,(x + third, y),sixth); (UpRight, (x + third + sixth, y - sixth),sixth); (Up, (x + 2*third, y), third)]
        | _ -> []
    else []

let rec processEdges grid edges =
    match edges with
    | edge::rest -> processEdges grid (rest @ processEdge grid edge)
    | [] -> ()

let run : char[,] =
    let grid = Array2D.init<char> 80 20 (fun x y -> ' ')
    //let path = [(SouthEast, 3);(NorthEast,1);(East,1);(SouthWest,1)]
    //drawPath grid ((0,10),path) |> ignore

    //let line = (East, (0,10), 9)
    //drawLine grid line |> ignore

    let edge = (Up, (0,10), 54)
    let edges = processEdges grid [edge]

    grid

