module Day7Tests

open Xunit
open Day7

[<Fact>]
let ``Must correctly computer Day 7 Part 1 Answer``() =
    Assert.Equal(880726, computeDay7Part1Answer())
