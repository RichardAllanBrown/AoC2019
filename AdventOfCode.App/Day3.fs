module Day3

type Coordinate = { x: int; y: int }

let origin = { x = 0; y = 0 }

type Direction =
    | Up
    | Right
    | Down
    | Left

type WireSegment = { origin: Coordinate; direction: Direction; distance: int }

let calculateDestination(w: WireSegment): Coordinate =
    match w.direction with
    | Up -> { x = w.origin.x; y = w.origin.y + w.distance }
    | Right -> { x = w.origin.x + w.distance; y = w.origin.y }
    | Down -> { x = w.origin.x; y = w.origin.y - w.distance }
    | Left -> { x = w.origin.x - w.distance; y = w.origin.y }

let isVertical(d: Direction): bool =
    match d with
    | Up -> true
    | Down -> true
    | _ -> false

let isHorizontal(d: Direction): bool = isVertical(d) |> not

let rec between(oneEnd: int, otherEnd: int)(value: int): bool =
    if (oneEnd <= otherEnd) then oneEnd <= value && value <= otherEnd
    else between(otherEnd, oneEnd)(value)

let hasCrossingPoint(vertical: WireSegment, horizontal: WireSegment): bool =
    let xAxisWithinBounds = between(horizontal.origin.x, calculateDestination(horizontal).x)(vertical.origin.x)
    let yAxisWithinBounds = between(vertical.origin.y, calculateDestination(vertical).y)(horizontal.origin.y)
    xAxisWithinBounds && yAxisWithinBounds

let calculateCrossingPoint(first: WireSegment, second: WireSegment): Coordinate option =
    let verticalWire = [first; second] |> List.tryFind(fun w -> isVertical(w.direction))
    let horizontalWire = [first; second] |> List.tryFind(fun w -> isHorizontal(w.direction))

    match (verticalWire, horizontalWire) with
    | (Some(vert), Some(hor)) when vert <> hor && hasCrossingPoint(vert, hor) -> Some({ x = vert.origin.x; y = hor.origin.y })
    | _ -> None

let toList =
    function
    | Some a -> a :: []
    | None -> []

let calculateAllCrossingPoints(firstWire: WireSegment list, secondWire: WireSegment list): Coordinate list =
    let crossingPointOpts = [
        for firstSeg in firstWire do
        for otherSeg in secondWire do
            calculateCrossingPoint(firstSeg, otherSeg)
    ]
    crossingPointOpts |> List.collect(toList)
    
let manhattenDistanceFromOrigin(coord: Coordinate): int = abs(coord.x) + abs(coord.y)

let parseDirection(c: char): Direction option =
    match c with
    | 'U' -> Some(Up)
    | 'R' -> Some(Right)
    | 'D' -> Some(Down)
    | 'L' -> Some(Left)
    | _ -> None

let parseIt(previousSegments: WireSegment list)(raw: string) : WireSegment list =
    let direction = parseDirection(raw.Chars(0)).Value
    let source = previousSegments |> List.tryLast |> Option.map(calculateDestination) |> Option.defaultValue(origin)
    let distance = raw.Substring(1) |> int
    let newSegment = { origin = source; direction = direction; distance = distance }
    List.append(previousSegments)([newSegment])

let buildWireSegments(source: string): WireSegment list =
    source
    |> Utils.splitLine
    |> List.fold(parseIt)([])

let calculateClosestCrossingPointDistance(oneWire: WireSegment list, otherWire: WireSegment list) : int = 
    calculateAllCrossingPoints(oneWire, otherWire)
    |> List.filter(fun c -> c <> origin)
    |> List.map(manhattenDistanceFromOrigin)
    |> List.min

let computeDay3Part1Answer(): int =
    let wires = Utils.readInputFile("./input/Day3.txt") |> List.map(buildWireSegments)
    calculateClosestCrossingPointDistance(wires.Item(0), wires.Item(1))

let computeDay3Part2Answer(): int =
    // calculate all crossingpoints less origin with wires that the crossing happened at
    // compute wires 
    0
