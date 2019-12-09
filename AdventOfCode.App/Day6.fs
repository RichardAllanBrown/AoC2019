module Day6

type Orbits = Map<string, Set<string>>

let private add a b = a + b

let rec private computeOrbitsForSelfAndChildren(orbits: Orbits)(level: int)(entity: string): int =
    orbits
    |> Map.tryFind(entity)
    |> Option.defaultValue(Set.empty)
    |> Set.toList
    |> List.sumBy(computeOrbitsForSelfAndChildren(orbits)(level + 1))
    |> add level

let computeTotalOrbits(orbits: Orbits) = computeOrbitsForSelfAndChildren(orbits)(0)("COM")

let addOrbit(orbits: Orbits)(orbit: string * string) =
    let (entity, satellite) = orbit
    let existingSatellites = orbits.TryFind(entity) |> Option.defaultValue(Set.empty)
    orbits.Add(entity, existingSatellites.Add(satellite))

let buildOrbitMap(orbits: (string * string) list) = orbits |> List.fold(addOrbit)(Map.empty)

let private splitOrbitInput(row: string): (string * string) =
    match row.Split(')') with
    | [| entity; satellite |] -> (entity, satellite)
    | _ -> invalidArg "row" "Invalid row format used"

let computeDay5Part1Answer(): int = 
    Utils.readInputFile("./input/Day6.txt")
    |> List.map(splitOrbitInput)
    |> buildOrbitMap
    |> computeTotalOrbits
