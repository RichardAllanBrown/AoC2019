module Utils

open System.IO

let readInputFile(fileName: string) : string list =
    File.ReadAllLines(fileName)
    |> List.ofSeq

let splitLine(str: string) : string list =
    str.Split(',') 
    |> List.ofSeq
