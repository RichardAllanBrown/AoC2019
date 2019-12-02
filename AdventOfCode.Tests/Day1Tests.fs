module Tests

open Xunit
open Day1

[<Theory>]
[<InlineData(12, 2)>]
[<InlineData(14, 2)>]
[<InlineData(1969, 654)>]
[<InlineData(100756, 33583)>]
let ``Calculate fuel for single ship`` (mass: int, expectedFuel: int) =
    Assert.Equal(expectedFuel, getFuelNeededToLiftMass(mass))

[<Fact>]
let ``Calculate fuel for many ships using basic calculation`` () =
    Assert.Equal(51, getTotalFuelForShips(getFuelNeededToLiftMass)(["123"; "12"; "21"; "23"]))

[<Fact>]
let ``Computes day 1 solution correctly`` () =
    Assert.Equal(3296269, computeDay1Part1Answer())

[<Theory>]
[<InlineData(14, 2)>]
[<InlineData(1969, 966)>]
[<InlineData(100756, 50346)>]
let ``Calculate fuel for single ship and fuel itself`` (mass: int, expectedFuel: int) =
    Assert.Equal(expectedFuel, getFuelNeededToLiftMassAndFuel(mass))

[<Fact>]
let ``Calculate fuel for many ships using advanced calculation`` () =
    Assert.Equal(63, getTotalFuelForShips(getFuelNeededToLiftMassAndFuel)(["123"; "12"; "21"; "23"]))

[<Fact>]
let ``Computes day 2 solution correctly`` () =
    Assert.Equal(4941547, computeDay1Part2Answer())
