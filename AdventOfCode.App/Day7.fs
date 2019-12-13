module Day7

open Intcode

module List = 
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

type Signal = {
    A: int
    B: int
    C: int
    D: int
    E: int
}

module Signal =
    let parse(values: int list): Signal =
        match values with
        | [a; b; c; d; e] -> { A = a; B = b; C = c; D = d; E = e }
        | _ -> invalidArg "values" "Must have exactly five arguments"

let runPossibleInput(initialMem: int list)(s: Signal): int = 
    let compA = Computer.build(initialMem, [s.A;0])
    let resultA = runToCompletion(compA) |> getLastOutput
    let compB = Computer.build(initialMem, [s.B;resultA])
    let resultB = runToCompletion(compB) |> getLastOutput
    let compC = Computer.build(initialMem, [s.C;resultB])
    let resultC = runToCompletion(compC) |> getLastOutput
    let compD = Computer.build(initialMem, [s.D;resultC])
    let resultD = runToCompletion(compD) |> getLastOutput
    let compE = Computer.build(initialMem, [s.E;resultD])
    runToCompletion(compE) |> getLastOutput
    
let computeDay7Part1Answer(): int =
    let initialMem = Utils.readInputFile("./input/Day7.txt") |> List.head |> Utils.splitLine |> List.map(int)
    [0;1;2;3;4]
    |> List.permutations
    |> Seq.toList
    |> Seq.map(Signal.parse)
    |> Seq.map(runPossibleInput(initialMem))
    |> Seq.max
