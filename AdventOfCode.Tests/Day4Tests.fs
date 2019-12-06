module Day4Tests

open Day4;
open Xunit;

[<Fact>]
let ``Computes correct day 4 part 1 answer``() = Assert.Equal(1855, computeDay4Part1Answer())

[<Fact>]
let ``Computes correct day 4 part 2 answer``() = Assert.Equal(1253, computeDay4Part2Answer())
