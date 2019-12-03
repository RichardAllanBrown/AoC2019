module Day2Tests

open Xunit
open Day2

let runProgramAndCheckFinalState(program: int list, expectedResult: int list) = 
    let initialState = { Program = program; Position = 0 }
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
let ``Correctly solves puzzle input``() = 
    Assert.Equal(2890696, computeDay2Part1Answer())
