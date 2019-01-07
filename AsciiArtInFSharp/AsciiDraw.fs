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
    List.last intermediatepoints


type path = point * (linedir * length) list

let drawPath (grid:char[,]) (start, steps) : point =
    List.fold (fun (lastpoint::endpoints) (dir, length) -> (drawLine grid (dir, lastpoint, length))::lastpoint::endpoints) [start] steps |> List.last






