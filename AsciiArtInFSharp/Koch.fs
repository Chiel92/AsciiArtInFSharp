module Koch

open Microsoft.FSharp.Collections
open AsciiDraw
open System

#if DEBUG
let (|>) value func =
  let result = func value
  result
#endif

// We assume that we always draw clockwise
// The snowflakeside is the direction in which there has to be constructed a new wedge
type snowflakeside = Up = 0 | UpRight = 1 | DownRight = 2 | Down = 3 | DownLeft = 4 | UpLeft = 5
// snowflakeside, start, length
type edge = snowflakeside * point * int

let edgeToLine (side, start, length) =
    let dir =
        match side with
        | snowflakeside.Up -> East
        | snowflakeside.UpRight -> SouthEast
        | snowflakeside.DownRight -> SouthWest
        | snowflakeside.Down -> West
        | snowflakeside.DownLeft -> NorthWest
        | snowflakeside.UpLeft -> NorthEast
        | _ -> failwith "Invalid snowflake side"
    let actualLength = if side = snowflakeside.Up || side = snowflakeside.Down then length * 2 else length
    (dir, start, actualLength)

let modulo m n = ((n % m) + m) % m

let getside (offset:int) (side : snowflakeside) : snowflakeside = LanguagePrimitives.EnumToValue side |> (+) offset |> modulo 6 |> LanguagePrimitives.EnumOfValue
let nextside : (snowflakeside -> snowflakeside) = getside 1
let previousside = getside -1

// TODO: don't draw the edges just yet, but gather them and flush at the end in the right order
let kochpattern side start length = 
    let edge1 = (side, start, length)
    let start2 = edge1 |> edgeToLine |> lineEnd
    let edge2 = (previousside side, start2, length)
    let start3 = edge2 |> edgeToLine |> lineEnd
    let edge3 = (nextside side, start3, length)
    let start4 = edge3 |> edgeToLine |> lineEnd
    let edge4 = (side, start4, length)
    edge4 |> edgeToLine |> lineEnd |> ignore
    [edge1; edge2; edge3; edge4]

let processEdge edge =
    let (side, start, length) = edge
    let third = length / 3
    if third > 0 then
        kochpattern side start third
    else []

let rec processEdges edges =
    match edges with
    | edge::rest -> 
        let result = processEdge edge
        // if we encounter an empty expansion, we quit, so the expansions must be done in the right order
        if not result.IsEmpty then processEdges (rest @ result) else edges
    | [] -> []

let run : char[,] =
    let grid = Array2D.init<char> 80 30 (fun x y -> ' ')

    let edges = [
            (snowflakeside.Up, (20,10), 18);
            (snowflakeside.DownRight, (56,10), 18);
            (snowflakeside.DownLeft, (38, 28), 18);
        ]
    let edgesToDraw = processEdges edges
     // sort result s.t. Up and Down are drawn first
    let lines = edgesToDraw |> List.sortBy (fun (dir, _, _) -> if dir = snowflakeside.Up || dir = snowflakeside.Down then 0 else 1) |> List.map edgeToLine
    try
        List.map (drawLine grid) lines |> ignore
    with
        | :? IndexOutOfRangeException as e -> Console.WriteLine e.Message

    grid

