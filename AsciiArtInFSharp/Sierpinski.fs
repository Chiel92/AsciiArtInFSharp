
module Sierpinski

open System
open Microsoft.FSharp.Collections

//TXTTXTTXTTXTTXTTXTTXTTXTTXT
//BLKBLKBLKBLKBLKBLKBLKBLKBLK
//TXT   TXTTXT   TXTTXT   TXT
//BLK   BLKBLK   BLKBLK   BLK
//TXTTXTTXTTXTTXTTXTTXTTXTTXT
//BLKBLKBLKBLKBLKBLKBLKBLKBLK
//TXTTXTTXT         TXTTXTTXT
//BLKBLKBLK         BLKBLKBLK
//TXT   TXT         TXT   TXT
//BLK   BLK         BLK   BLK
//TXTTXTTXT         TXTTXTTXT
//BLKBLKBLK         BLKBLKBLK
//TXTTXTTXTTXTTXTTXTTXTTXTTXT
//BLKBLKBLKBLKBLKBLKBLKBLKBLK
//TXT   TXTTXT   TXTTXT   TXT
//BLK   BLKBLK   BLKBLK   BLK
//TXTTXTTXTTXTTXTTXTTXTTXTTXT
//BLKBLKBLKBLKBLKBLKBLKBLKBLK

let conc (x,y,z) = x + y + z
let blanks (s:string) = Seq.map (fun _ -> ' ') s |> String.Concat

let rec sierpinski (n:int) : string list =
    match n with 
    | 0 -> ["###";"###"]
    | _ -> 
        let x = sierpinski (n-1)
        let y = List.map blanks x
        let part1 = List.zip3 x x x |> List.map conc
        let part2 = List.zip3 x y x |> List.map conc
        part1 @ part2 @ part1


