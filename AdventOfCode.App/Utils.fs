module Utils

open System.IO

let readInputFile(fileName: string) : string list =
    File.ReadAllLines(fileName)
    |> List.ofSeq

let splitLine(str: string) : string list =
    str.Split(',') 
    |> List.ofSeq

let updateElementAt(indexToUpdate: int, newValue: 'a)(originalList: 'a list): 'a list = 
    originalList  
    |> List.indexed 
    |> List.map(fun (i, v) -> if (i = indexToUpdate) then newValue else v)
