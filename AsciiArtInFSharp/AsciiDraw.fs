module AsciiDraw

open System
open Microsoft.FSharp.Collections

let rec private generateSequence succ state length = if length > 0 then state::(generateSequence succ (succ state)) (length-1) else []

// Our coordinate system is the standard screen coordinate system. The points
// are the corners of the character tiles. The allowed line characters are /\_.
// The order in which things are drawn matters of course, because line
// characters may overlap. Generally speaking, the _ characters should be drawn
// first, because they have a lot of whitespace and may therefore suggest
// unintended gaps. This is the main reason not to draw on the grid, but gather
// the lines in a list and draw them in the right order at the end of the
// process.

type linedir = East | SouthEast | SouthWest | West | NorthWest | NorthEast
type point = int * int
type length = int
type line = linedir * point * length

let private linedirSucc dir = 
    match dir with
    | East -> fun (x,y) -> (x+1,y)
    | SouthEast -> fun (x,y) -> (x+1,y+1)
    | SouthWest -> fun (x,y) -> (x-1,y+1)
    | West -> fun (x,y) -> (x-1,y)
    | NorthWest-> fun (x,y) -> (x-1,y-1)
    | NorthEast -> fun (x,y) -> (x+1,y-1)

let public lineEnd (l:line) : point =
    let dir, (x,y), length = l
    let next = linedirSucc dir
    let intermediatepoints = generateSequence next (x,y) (length + 1)
    List.last intermediatepoints

let public drawLine (grid:char[,]) (l:line) = 
    let dir, (x,y), length = l
    let startingcharpos, character = 
        match dir with
        | East -> (x,y-1), '_'
        | SouthEast -> (x,y), '\\'
        | SouthWest -> (x-1,y), '/'
        | West -> (x-1,y-1), '_'
        | NorthWest-> (x-1,y-1), '\\'
        | NorthEast -> (x,y-1), '/'
    let next = linedirSucc dir
    generateSequence next startingcharpos length |> List.map (fun (x,y) -> grid.[x,y] <- character) |> ignore
