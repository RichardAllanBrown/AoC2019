module Day3Tests

open Day3;
open Xunit;
open TestUtils;

type CalculateDestinationTestData() = 
    inherit ClassDataBase([
        [| { origin = { x = 0; y = 0 }; direction = Up; distance = 1 }; { x = 0; y = 1 } |];
        [| { origin = { x = 100; y = 20 }; direction = Right; distance = 23 }; { x = 123; y = 20 } |];
        [| { origin = { x = 1000; y = 450 }; direction = Down; distance = 12 }; { x = 1000; y = 438 } |];
        [| { origin = { x = 760; y = 340 }; direction = Left; distance = 32 }; { x = 728; y = 340 } |]
    ])

[<Theory; ClassData(typeof<CalculateDestinationTestData>)>]
let ``Calculate destination from origin returns expected``(wire: WireSegment, dest: Coordinate) =
    Assert.Equal(dest, calculateDestination(wire))

[<Theory>]
[<InlineData(4, 8, 5, true)>]
[<InlineData(8, 4, 5, true)>]
[<InlineData(4, 8, 1, false)>]
[<InlineData(1, 2, 1, true)>]
[<InlineData(1, 2, 5, false)>]
let ``Between calulcates expected result``(oneEnd: int, otherEnd: int, value: int, expected: bool) =
    Assert.Equal(expected, between(oneEnd, otherEnd)(value))

type CalulateCrossingPointTestData() = 
    inherit ClassDataBase([
        [| { origin = { x = 1; y = 1 }; direction = Up; distance = 5 };
            { origin = { x = 0; y = 4 }; direction = Right; distance = 12 };
            { x = 1; y = 4 } |];
        [| { origin = { x = 0; y = 4 }; direction = Right; distance = 12 };
            { origin = { x = 1; y = 1 }; direction = Up; distance = 5 };            
            { x = 1; y = 4 } |];
        [| { origin = { x = 0; y = 0 }; direction = Down; distance = 12 };
            { origin = { x = 0; y = 0 }; direction = Left; distance = 5 };            
            { x = 0; y = 0 } |];
        [| { origin = { x = 0; y = 0 }; direction = Right; distance = 51 };
            { origin = { x = 4; y = 12 }; direction = Down; distance = 35 };            
            { x = 4; y = 0 } |]
    ])

[<Theory; ClassData(typeof<CalulateCrossingPointTestData>)>]
let ``Crossing coordinate can be calculated``(oneWire: WireSegment, otherWire: WireSegment, expectedCrossing: Coordinate) =
    Assert.Equal(Some(expectedCrossing), calculateCrossingPoint(oneWire, otherWire))

[<Fact>]
let ``Works for simple test case``() =
    let oneWire = buildWireSegments("R75,D30,R83,U83,L12,D49,R71,U7,L72")
    let otherWire = buildWireSegments("U62,R66,U55,R34,D71,R55,D58,R83")
    Assert.Equal(159, calculateClosestCrossingPointDistance(oneWire, otherWire))

[<Fact>]
let ``Works for a slightly more complex test case``() =
    let oneWire = buildWireSegments("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
    let otherWire = buildWireSegments("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    Assert.Equal(135, calculateClosestCrossingPointDistance(oneWire, otherWire))

[<Fact>]
let ``Calculates correct answer for part 1 input``() = Assert.Equal(1064, computeDay3Part1Answer())
