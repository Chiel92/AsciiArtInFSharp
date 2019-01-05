module AsciiDraw
open System
open Microsoft.FSharp.Collections

let rec generateSequence succ state length = if length > 0 then state::(generateSequence succ (succ state)) (length-1) else []

// We need a result grid and a structure that holds the things we still need to do, such that we don't have to search in the grid
// Our coordinate system is the standard screen coordinate system
// The points are the corners of the character tiles. The allowed line characters are /\_
// The order in which things are drawn matters of course, because line characters may overlap
// Generally speaking, the _ characters should be drawn first, because they have a lot of whitespace and may therefore suggest
// unintended gaps

type linedir = East | SouthEast | SouthWest | West | NorthWest | NorthEast
type point = int * int
type length = int
type line = linedir * point * length

let drawLine (grid:char[,]) (l:line) : point = 
    let dir, (x,y), length = l
    let startingcharpos = 
        match dir with
        | East -> (x,y-1)
        | SouthEast -> (x,y)
        | SouthWest -> (x-1,y)
        | West -> (x-1,y-1)
        | NorthWest-> (x-1,y-1)
        | NorthEast -> (x,y-1)
    let character = 
        match dir with
        | East -> '_'
        | SouthEast -> '\\'
        | SouthWest -> '/'
        | West -> '_'
        | NorthWest-> '\\'
        | NorthEast -> '/'
    let next = 
        match dir with
        | East -> fun (x,y) -> (x+1,y)
        | SouthEast -> fun (x,y) -> (x+1,y+1)
        | SouthWest -> fun (x,y) -> (x-1,y+1)
        | West -> fun (x,y) -> (x-1,y)
        | NorthWest-> fun (x,y) -> (x-1,y-1)
        | NorthEast -> fun (x,y) -> (x+1,y-1)
    generateSequence next startingcharpos length |> List.map (fun (x,y) -> grid.[x,y] <- character) |> ignore
    let intermediatepoints = generateSequence next (x,y) (length + 1)
    Console.WriteLine(intermediatepoints.ToString())
    List.last intermediatepoints


type path = point * (linedir * length) list

let drawPath (grid:char[,]) (start, steps) : point =
    List.fold (fun (lastpoint::endpoints) (dir, length) -> (drawLine grid (dir, lastpoint, length))::lastpoint::endpoints) [start] steps |> List.last







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
