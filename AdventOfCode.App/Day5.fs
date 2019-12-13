module Day5

open Intcode

let runIntcode(input: int)(program: int list): int =
    Computer.build(program, List.singleton(input))
    |> runToCompletion
    |> getLastOutput

let computeDay5Part1Answer(): int =
    Utils.readInputFile("./input/Day5.txt")
    |> List.head 
    |> Utils.splitLine
    |> List.map(int)
    |> runIntcode(1)
    
let computeDay5Part2Answer(): int =
    Utils.readInputFile("./input/Day5.txt")
    |> List.head 
    |> Utils.splitLine
    |> List.map(int)
    |> runIntcode(5)
