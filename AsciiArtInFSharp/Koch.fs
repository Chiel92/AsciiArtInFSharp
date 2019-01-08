module Koch

// TODO: use a monad to draw stuff

open Microsoft.FSharp.Collections
open AsciiDraw
open System

#if DEBUG
let (|>) value func =
  let result = func value
  result
#endif


// Clockwise direction, start, size of snowflake
type Edge = Direction * Point * int

// We assume that we always draw clockwise
let edgeToLine (dir, start, size) =
    let length = if dir = East || dir = West then size * 2 else size
    (dir, start, length)

// The SnowFlakedir is the direction in which there has to be constructed a new wedge
let kochpattern dir start size = 
    let edge1 = (dir, start, size)
    let start2 = edge1 |> edgeToLine |> lineEnd
    let edge2 = (previousDir dir, start2, size)
    let start3 = edge2 |> edgeToLine |> lineEnd
    let edge3 = (nextDir dir, start3, size)
    let start4 = edge3 |> edgeToLine |> lineEnd
    let edge4 = (dir, start4, size)
    [edge1; edge2; edge3; edge4]

let processEdge (dir, start, size) =
    let nextsize = size / 3
    if nextsize > 0 then
        Some (kochpattern dir start nextsize)
    else None

let rec processEdges edges =
    match edges with
    | edge::rest -> 
        // If we fail to expand an edge, we quit, so the expansions must be done in the right order
        match processEdge edge with
        | Some result -> processEdges (rest @ result)
        | None -> edges
    | [] -> []

let run : char[,] =
    let order = 3

    let size = pown 3 order
    // A snowflake of size n needs 2n x n+1 space
    let grid = Array2D.init<char> (2 * size) (size + size / 3) (fun x y -> ' ')

    let start = (0, size / 3)
    let edge1 = (East, start, size);
    let end1 = edge1 |> edgeToLine |> lineEnd
    let edge2 = (SouthWest, end1, size);
    let end2 = edge2 |> edgeToLine |> lineEnd
    let edge3 = (NorthWest, end2, size);
    let edges = [ edge1; edge2; edge3 ]

    let edgesToDraw = processEdges edges
     // Sort result s.t. Up and Down are drawn first
    let lines = edgesToDraw |> List.sortBy (fun (dir, _, _) -> if dir = East || dir = West then 0 else 1) |> List.map edgeToLine
    try
        List.map (drawLine grid) lines |> ignore
    with
        | :? IndexOutOfRangeException as e -> Console.WriteLine e.Message

    grid

