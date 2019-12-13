module Intcode

type Program = {
    Memory: int list
    Position: int
    Halted: bool
}

module Program =
    let build(mem: int list) =
        { Memory = mem; Position = 0; Halted = false }

type Computer = {
    Program: Program
    Input: int list
    Output: int list
}

module Computer = 
    let build(mem: int list, input: int list) = 
        { Program = Program.build(mem); Input = input; Output = List.empty }

type OpCode = 
    | Add
    | Multiply
    | ReadInput
    | WriteOutput
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | Halt

module OpCode = 
    let parse(rawCode: int) =
        match rawCode with
        | 1 -> Add
        | 2 -> Multiply
        | 3 -> ReadInput
        | 4 -> WriteOutput
        | 5 -> JumpIfTrue
        | 6 -> JumpIfFalse
        | 7 -> LessThan
        | 8 -> Equals
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

let private getValue(m: ParameterMode, offset: int)(s: Program) : int =
    let firstVal = s.Memory.Item(s.Position + offset)
    match m with
    | Immediate -> firstVal
    | Position -> s.Memory.Item(firstVal)

let private putValue(m: ParameterMode, offset: int, newValue: int)(s: Program): int list =
    let outIndex = s.Position + offset
    match m with
    | Immediate -> Utils.updateElementAt(outIndex, newValue)(s.Memory)
    | Position -> Utils.updateElementAt(s.Memory.Item(outIndex), newValue)(s.Memory)
    
let private execute2To1Instruction(f: int -> int -> int)(inputAMode: ParameterMode, inputBMode: ParameterMode, outputMode: ParameterMode)(s: Computer): Computer =
    let inputA = getValue(inputAMode, 1)(s.Program)
    let inputB = getValue(inputBMode, 2)(s.Program)
    let result = f(inputA)(inputB)
    let newProgram = putValue(outputMode, 3, result)(s.Program)
    { Input = s.Input; Program = { Memory = newProgram; Position = s.Program.Position + 4; Halted = false }; Output = s.Output }

let private executeAdd = execute2To1Instruction(fun a b -> a + b)

let private executeMultiply = execute2To1Instruction(fun a b -> a * b)

let private executeReadInput(outputMode: ParameterMode)(computer: Computer): Computer =
    let newMemory = putValue(outputMode, 1, List.head(computer.Input))(computer.Program)
    { Input = List.tail(computer.Input); Program = { Memory = newMemory; Position = computer.Program.Position + 2; Halted = false }; Output = computer.Output }
    
let private executeWriteOutput(inputMode: ParameterMode)(s: Computer): Computer =
    let valueToOutput = getValue(inputMode, 1)(s.Program)
    let output = valueToOutput :: s.Output
    { Input = s.Input; Program = { Memory = s.Program.Memory; Position = s.Program.Position + 2; Halted = false }; Output = output }

let private executeJumpIfTrue(inputMode: ParameterMode, pointerArgMode: ParameterMode)(s: Computer): Computer =
    let newPosition = 
        match getValue(inputMode, 1)(s.Program) with
        | 0 -> s.Program.Position + 3
        | _ -> getValue(pointerArgMode, 2)(s.Program)
    { Input = s.Input; Program = { Memory = s.Program.Memory; Position = newPosition; Halted = false }; Output = s.Output }

let private executeJumpIfFalse(inputMode: ParameterMode, pointerArgMode: ParameterMode)(s: Computer): Computer =
    let newPosition = 
        match getValue(inputMode, 1)(s.Program) with
        | 0 -> getValue(pointerArgMode, 2)(s.Program)
        | _ -> s.Program.Position + 3
    { Input = s.Input; Program = { Memory = s.Program.Memory; Position = newPosition; Halted = false }; Output = s.Output }

let private executeLessThan = execute2To1Instruction(fun a b -> if (a < b) then 1 else 0)

let private executeEquals = execute2To1Instruction(fun a b -> if (a = b) then 1 else 0)

let private executeHalt(computer: Computer): Computer =
    { Input = computer.Input; Program = { Memory = computer.Program.Memory; Position = computer.Program.Position; Halted = true }; Output = computer.Output }

let computeNextState(current: Computer): Computer =
    let currentInstruction = parseInstruction(current.Program.Memory.Item(current.Program.Position));
    match currentInstruction with
    | { OpCode = Add; FirstArgMode = inputAMode; SecondArgMode = inputBMode; ThirdArgMode = outputMode } ->
        executeAdd(inputAMode, inputBMode, outputMode)(current)
    | { OpCode = Multiply; FirstArgMode = inputAMode; SecondArgMode = inputBMode; ThirdArgMode = outputMode } -> 
        executeMultiply(inputAMode, inputBMode, outputMode)(current)
    | { OpCode = ReadInput; FirstArgMode = outputMode } ->
        executeReadInput(outputMode)(current)
    | { OpCode = WriteOutput; FirstArgMode = inputMode } ->
        executeWriteOutput(inputMode)(current)
    | { OpCode = JumpIfTrue; FirstArgMode = inputMode; SecondArgMode = pointerArgMode } ->
        executeJumpIfTrue(inputMode, pointerArgMode)(current)
    | { OpCode = JumpIfFalse; FirstArgMode = inputMode; SecondArgMode = pointerArgMode } ->
        executeJumpIfFalse(inputMode, pointerArgMode)(current)
    | { OpCode = LessThan; FirstArgMode = inputAMode; SecondArgMode = inputBMode; ThirdArgMode = outputMode } ->
        executeLessThan(inputAMode, inputBMode, outputMode)(current)
    | { OpCode = Equals; FirstArgMode = inputAMode; SecondArgMode = inputBMode; ThirdArgMode = outputMode } ->
        executeEquals(inputAMode, inputBMode, outputMode)(current)
    | { OpCode = Halt } -> executeHalt(current)

let rec runToCompletion(computer: Computer): Computer =
    let nextState = computeNextState(computer)
    if (nextState.Program.Halted) then nextState else runToCompletion(nextState)

let getLastOutput(computer: Computer): int = computer.Output.Head
