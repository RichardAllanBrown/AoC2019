module Day8

module List = 
    let rec sliding(size: int)(l: 'a list): 'a list list = 
        if (l.IsEmpty) then []
        else List.take(size)(l) :: sliding(size)(List.skip(size)(l))

let computeImageLayers(width: int, height: int)(data: 'a list): 'a list list =
    data |> List.sliding(width * height)

let countInstancesOf value lst = 
    let fitleredList = List.filter(fun s -> s = value)(lst)
    fitleredList.Length

let computeAnswer(l: char list): int = 
    let countof1 = countInstancesOf('1')(l)
    let countOf2 = countInstancesOf('2')(l)
    countof1 * countOf2

let computeDay8Part1Answer(): int =
    Utils.readInputFile("./input/Day8.txt") 
    |> List.head
    |> Seq.toList
    |> computeImageLayers(25, 6)
    |> List.minBy(countInstancesOf('0'))
    |> computeAnswer