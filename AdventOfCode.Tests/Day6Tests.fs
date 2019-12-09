module Day6Tests

open Xunit
open Day6
open TestUtils

let exampleOrbitMap = [
        ("COM", "B");
        ("B", "C");
        ("C", "D");
        ("D", "E");
        ("E", "F");
        ("B", "G");
        ("G", "H");
        ("D", "I");
        ("E", "J");
        ("J", "K");
        ("K", "L");
    ]

type IdentifyDirectSatellitesData() = 
    inherit ClassDataBase([
        [| "COM"; Set.ofList(["B"]) |];
        [| "D"; Set.ofList(["E"; "I"]) |];
    ])

[<Theory; ClassData(typeof<IdentifyDirectSatellitesData>)>]
let ``Parsed orbit map allows for identifying direct satellites``(entity: string, expectedSatellites: Set<string>) =
    let satellites = exampleOrbitMap |> buildOrbitMap |> Map.find(entity)
    Assert.Equal<Set<string>>(expectedSatellites, satellites)

[<Fact>]
let ``Computes day 6 part 1 answer``() = Assert.Equal(142497, computeDay5Part1Answer())
