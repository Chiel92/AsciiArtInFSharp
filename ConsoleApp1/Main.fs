module Main

open System

[<EntryPoint>]
let main argv =
    let grid :char[,] = AsciiDraw.run
    let lines = [|for i in [0..(Array2D.length2 grid) - 1] -> grid.[*,i] |> String.Concat |]
    Console.Write(String.Join(System.Environment.NewLine, lines))
    Console.Read() |> ignore
    0 // return an integer exit code
