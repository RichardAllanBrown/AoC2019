module Day2

type ComputerState = {
    Program: int list
    Position: int
}

type OpCode = 
    | Halt
    | CalculateOp of (int -> int -> int)
    | Unknown

let parseOpCode(opCode: int): OpCode =
    match opCode with
        | 1 -> CalculateOp(fun a b -> a + b)
        | 2 -> CalculateOp(fun a b -> a * b)
        | 99 -> Halt
        | _ -> Unknown

let getValueAt(state: ComputerState)(index: int): int = state.Program.Item(index)

let getValueAtOffset(state: ComputerState)(offset: int): int = getValueAt(state)(state.Position + offset)

let updateElementAt(indexToUpdate: int, newValue: 'a)(originalList: 'a list): 'a list = 
    originalList  
    |> List.indexed 
    |> List.map(fun (i, v) -> if (i = indexToUpdate) then newValue else v)

let applyOp(state: ComputerState, op: int -> int -> int): ComputerState =
    let getValueAt = getValueAt(state)
    let getValueAtOffset = getValueAtOffset(state)
    let firstValue = getValueAt(getValueAtOffset(1))
    let secondValue = getValueAt(getValueAtOffset(2))
    let newValue = op(firstValue)(secondValue)
    let newProgram = updateElementAt(getValueAtOffset(3), newValue)(state.Program)
    { Position = state.Position + 4; Program = newProgram }

let computeNextState(current: ComputerState) : ComputerState =
    let getValueAtOffset = getValueAtOffset(current)
    match parseOpCode(getValueAtOffset(0)) with
        | Halt-> current
        | CalculateOp op -> applyOp(current, op)
        | Unknown -> failwith "Unrecognised op code"

let rec runToCompletion(initial: ComputerState): ComputerState list =
    let currentStates = List.singleton(initial)
    let nextState = computeNextState(initial)
    if (nextState = initial) then currentStates else List.append(currentStates)(runToCompletion(nextState))

let getFinalValue(states: ComputerState list): int = List.last(states).Program.Item(0)

let runIntcode(program: int list): int =
    let initialState = { Program = program; Position = 0 }
    let completedProgram = runToCompletion(initialState)    
    getFinalValue(completedProgram)

let computeDay2Part1Answer(): int =
    Utils.readInputFile("./input/Day2.txt")
    |> List.head 
    |> Utils.splitLine
    |> List.map(int)
    |> updateElementAt(1, 12)
    |> updateElementAt(2, 2)
    |> runIntcode
