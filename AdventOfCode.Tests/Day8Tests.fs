module Day8Tests

open Xunit
open Day8

[<Fact>]
let ``Computes day 8 part 1 answer``() = 
    Assert.Equal(1452, computeDay8Part1Answer())
