module Day5

type ComputerState = {
    Input: int
    Program: int list
    Output: int option
    Position: int
}

module ComputerState =
    let parse(program: int list, input: int) =
        { Input = input; Program = program; Output = None; Position = 0; }

type OpCode = 
    | Add
    | Multiply
    | ReadInput
    | WriteOutput
    | Halt

module OpCode = 
    let parse(rawCode: int) =
        match rawCode with
        | 1 -> Add
        | 2 -> Multiply
        | 3 -> ReadInput
        | 4 -> WriteOutput
        | 99 -> Halt
        | _ -> invalidArg "rawCode" "Invalid op code" 

type ParameterMode = Position | Immediate

module ParameterMode =
    let parse(rawMode: int) =
        match rawMode with
        | 0 -> Position
        | 1 -> Immediate
        | _ -> invalidArg "rawMode" "Invalid parameter mode"

type Instruction = {
    OpCode: OpCode
    FirstArgMode: ParameterMode
    SecondArgMode: ParameterMode
    ThirdArgMode: ParameterMode
}

let parseInstruction(i: int) : Instruction = 
    let asString = string(i).PadLeft(5, '0')
    let opCode = asString.Substring(3, 2) |> int |> OpCode.parse
    let firstArgMode = asString.Substring(2, 1) |> int |> ParameterMode.parse
    let secondArgMode = asString.Substring(1, 1) |> int |> ParameterMode.parse
    let thirdArgMode = asString.Substring(0, 1) |> int |> ParameterMode.parse
    { OpCode = opCode; FirstArgMode = firstArgMode; SecondArgMode = secondArgMode; ThirdArgMode = thirdArgMode }

let private getValue(m: ParameterMode, offset: int)(s: ComputerState) : int =
    let firstVal = s.Program.Item(s.Position + offset)
    match m with
    | Immediate -> firstVal
    | Position -> s.Program.Item(firstVal)

let private putValue(m: ParameterMode, offset: int, newValue: int)(s: ComputerState): int list =
    let outIndex = s.Position + offset
    match m with
    | Immediate -> Utils.updateElementAt(outIndex, newValue)(s.Program)
    | Position -> Utils.updateElementAt(s.Program.Item(outIndex), newValue)(s.Program)

let private executeAdd(inputAMode: ParameterMode, inputBMode: ParameterMode, outputMode: ParameterMode)(s: ComputerState): ComputerState =
    let inputA = getValue(inputAMode, 1)(s)
    let inputB = getValue(inputBMode, 2)(s)
    let newProgram = putValue(outputMode, 3, inputA + inputB)(s)
    { Input = s.Input; Program = newProgram; Output = s.Output; Position = s.Position + 4 }

let private executeMultiply(inputAMode: ParameterMode, inputBMode: ParameterMode, outputMode: ParameterMode)(s: ComputerState): ComputerState =
    let inputA = getValue(inputAMode, 1)(s)
    let inputB = getValue(inputBMode, 2)(s)
    let newProgram = putValue(outputMode, 3, inputA * inputB)(s)
    { Input = s.Input; Program = newProgram; Output = s.Output; Position = s.Position + 4 }

let private executeReadInput(outputMode: ParameterMode)(s: ComputerState): ComputerState =
    let newProgram = putValue(outputMode, 1, s.Input)(s)
    { Input = s.Input; Program = newProgram; Output = s.Output; Position = s.Position + 2 }
    
let private executeWriteOutput(inputMode: ParameterMode)(s: ComputerState): ComputerState =
    let valueToOutput = getValue(inputMode, 1)(s)
    { Input = s.Input; Program = s.Program; Output = Some(valueToOutput); Position = s.Position + 2 }

let computeNextState(current: ComputerState): ComputerState =
    let currentInstruction = parseInstruction(current.Program.Item(current.Position));
    match currentInstruction with
    | { OpCode = Add; FirstArgMode = inputAMode; SecondArgMode = inputBMode; ThirdArgMode = outputMode } ->
        executeAdd(inputAMode, inputBMode, outputMode)(current)
    | { OpCode = Multiply; FirstArgMode = inputAMode; SecondArgMode = inputBMode; ThirdArgMode = outputMode } -> 
        executeMultiply(inputAMode, inputBMode, outputMode)(current)
    | { OpCode = ReadInput; FirstArgMode = outputMode } ->
        executeReadInput(outputMode)(current)
    | { OpCode = WriteOutput; FirstArgMode = inputMode } ->
        executeWriteOutput(inputMode)(current)
    | { OpCode = Halt } -> current

let rec runToCompletion(initial: ComputerState): ComputerState list =
    let currentStates = List.singleton(initial)
    let nextState = computeNextState(initial)
    if (nextState = initial) then currentStates else List.append(currentStates)(runToCompletion(nextState))

let getOutputValue(states: ComputerState list): int option = List.last(states).Output

let runIntcode(input: int)(program: int list): int =
    ComputerState.parse(program, input)
    |> runToCompletion
    |> getOutputValue
    |> Option.get

let computeDay5Part1Answer(): int =
    Utils.readInputFile("./input/Day5.txt")
    |> List.head 
    |> Utils.splitLine
    |> List.map(int)
    |> runIntcode(1)
