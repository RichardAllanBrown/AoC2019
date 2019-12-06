module Day5Tests

open Xunit
open Day5

let runProgramAndCheckFinalState(program: int list, expectedResult: int list) = 
    let initialState = ComputerState.parse(program, 0)
    let completedProgram = runToCompletion(initialState)
    let result = expectedResult = List.last(completedProgram).Program
    Assert.True(result)

[<Fact>]
let ``Simple addition program reaches expected end state``() =
    runProgramAndCheckFinalState([1; 0; 0; 0; 99], [2; 0; 0; 0; 99])

[<Fact>]
let ``Simple multiplication program reaches expected end state``() =
    runProgramAndCheckFinalState([2; 3; 0; 3; 99], [2; 3; 0; 6; 99])
    
[<Fact>]
let ``Multiplication with larger numbers``() =
    runProgramAndCheckFinalState([2; 4; 4; 5; 99; 0], [2; 4; 4; 5; 99; 9801])

[<Fact>]
let ``Overwriting a terminal op code reaches expected end state``() =
    runProgramAndCheckFinalState([1; 1; 1; 4; 99; 5; 6; 0; 99], [30; 1; 1; 4; 2; 5; 6; 0; 99])

[<Fact>]
let ``Correctly solves puzzle input for part 1``() = 
    Assert.Equal(5346030, computeDay5Part1Answer())

[<Fact>]
let ``Correctly solves puzzle input for part 2``() = 
    Assert.Equal(513116, computeDay5Part2Answer())
