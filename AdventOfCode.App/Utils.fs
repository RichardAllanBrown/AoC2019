module Utils

open System.IO

let readInputFile(fileName: string) : string list =
    File.ReadAllLines(fileName)
    |> List.ofSeq
