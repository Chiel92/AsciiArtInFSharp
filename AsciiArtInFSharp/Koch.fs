module Koch

open Microsoft.FSharp.Collections
open AsciiDraw
open System

// Clockwise direction, start, size of snowflake
type Edge = Direction * Point * int

// We assume that we always draw clockwise
let edgeToLine (dir, start, size) =
    let length = if dir = East || dir = West then size * 2 else size
    (dir, start, length)

// Operator for drawing paths
let (==>) (edges, start) (dir, size) = 
    let edge = (dir, start, size)
    (edge::edges, edge |> edgeToLine |> lineEnd)

let kochpattern dir start size = ([], start) ==> (dir, size) ==> (prevDir dir, size) ==> (nextDir dir, size) ==> (dir, size) |> fst

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
    let edges = ([], start) ==> (East, size) ==> (SouthWest, size) ==> (NorthWest, size) |> fst

    let edgesToDraw = processEdges edges
     // Sort result s.t. Up and Down are drawn first
    let lines = edgesToDraw |> List.sortBy (fun (dir, _, _) -> if dir = East || dir = West then 0 else 1) |> List.map edgeToLine
    try
        List.map (drawLine grid) lines |> ignore
    with
        | :? IndexOutOfRangeException as e -> Console.WriteLine e.Message

    grid
