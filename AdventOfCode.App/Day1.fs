module Day1

let getFuelNeededToLiftMass(mass: int) : int = (mass / 3) - 2

let rec getFuelNeededToLiftMassAndFuel(mass: int) : int =
    let fuelNeededForMass = getFuelNeededToLiftMass(mass)
    if (fuelNeededForMass <= 0) then 0 
    else fuelNeededForMass + getFuelNeededToLiftMassAndFuel(fuelNeededForMass)

let getTotalFuelForShips(calculateFuelRequirement: int -> int)(input: string list) : int = 
    input
    |> List.map(int)
    |> List.map(calculateFuelRequirement)
    |> List.sum

let computeDay1Part1Answer(): int = 
    Utils.readInputFile("./Input/Day1.txt")
    |> getTotalFuelForShips(getFuelNeededToLiftMass)

let computeDay1Part2Answer(): int = 
    Utils.readInputFile("./Input/Day1.txt")
    |> getTotalFuelForShips(getFuelNeededToLiftMassAndFuel)
