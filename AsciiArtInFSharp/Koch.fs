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

type SnowFlakeSide = Up = 0 | UpRight = 1 | DownRight = 2 | Down = 3 | DownLeft = 4 | UpLeft = 5

let modulo m n = ((n % m) + m) % m

let rotateSide (steps:int) (side:SnowFlakeSide) : SnowFlakeSide = LanguagePrimitives.EnumToValue side |> (+) steps |> modulo 6 |> LanguagePrimitives.EnumOfValue
let nextSide : (SnowFlakeSide -> SnowFlakeSide) = rotateSide 1
let previousSide = rotateSide -1


// SnowFlakeSide, start, size of snowflake
type Edge = SnowFlakeSide * point * int

// We assume that we always draw clockwise
let edgeToLine (side, start, size) =
    let dir =
        match side with
        | SnowFlakeSide.Up -> East
        | SnowFlakeSide.UpRight -> SouthEast
        | SnowFlakeSide.DownRight -> SouthWest
        | SnowFlakeSide.Down -> West
        | SnowFlakeSide.DownLeft -> NorthWest
        | SnowFlakeSide.UpLeft -> NorthEast
        | _ -> failwith "Invalid snowflake side"
    let length = if side = SnowFlakeSide.Up || side = SnowFlakeSide.Down then size * 2 else size
    (dir, start, length)

// The SnowFlakeSide is the direction in which there has to be constructed a new wedge
let kochpattern side start size = 
    let edge1 = (side, start, size)
    let start2 = edge1 |> edgeToLine |> lineEnd
    let edge2 = (previousSide side, start2, size)
    let start3 = edge2 |> edgeToLine |> lineEnd
    let edge3 = (nextSide side, start3, size)
    let start4 = edge3 |> edgeToLine |> lineEnd
    let edge4 = (side, start4, size)
    [edge1; edge2; edge3; edge4]

let processEdge (side, start, size) =
    let nextsize = size / 3
    if nextsize > 0 then
        Some (kochpattern side start nextsize)
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
    let edge1 = (SnowFlakeSide.Up, start, size);
    let end1 = edge1 |> edgeToLine |> lineEnd
    let edge2 = (SnowFlakeSide.DownRight, end1, size);
    let end2 = edge2 |> edgeToLine |> lineEnd
    let edge3 = (SnowFlakeSide.DownLeft, end2, size);
    let edges = [ edge1; edge2; edge3 ]

    let edgesToDraw = processEdges edges
     // Sort result s.t. Up and Down are drawn first
    let lines = edgesToDraw |> List.sortBy (fun (dir, _, _) -> if dir = SnowFlakeSide.Up || dir = SnowFlakeSide.Down then 0 else 1) |> List.map edgeToLine
    try
        List.map (drawLine grid) lines |> ignore
    with
        | :? IndexOutOfRangeException as e -> Console.WriteLine e.Message

    grid

