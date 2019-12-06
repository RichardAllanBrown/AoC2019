module Day4

type PasswordDigit = private PasswordDigit of int

module PasswordDigit =
    let create(qty: int) =
        if (qty < 0 || 9 < qty) then invalidArg "qty" "Must be a single digit" 
        else PasswordDigit(qty)

type Password = private Password of PasswordDigit list

module Password =
    let create(n: int) =
        let digits = string(n) |> Seq.toList |> List.map(string) |> List.map(int) |> List.map(PasswordDigit.create)
        Password(digits)

type ValidationRule = Password -> Result<Password, string>

let private validator(f: PasswordDigit list -> bool, errorMsg: string)(p: Password) =
    match p with
    | Password(digits) when f(digits) -> Ok p
    | _ -> Error errorMsg

let validateLength = validator((fun digits -> digits.Length = 6), "Length must be exactly 6")

let private hasNeighbouringDuplicate(list: 'a list): bool =
    list 
    |> List.pairwise 
    |> List.exists(fun (a, b) -> a = b)

let hasAdjacentDigits = validator(hasNeighbouringDuplicate, "Must contain a neighbouring pair of the same digit")

let private neverDecreasingRun(digits: PasswordDigit list): bool =
    digits
    |> List.pairwise
    |> List.forall(fun (PasswordDigit(last), PasswordDigit(current)) -> last <= current)

let neverDecreases = validator(neverDecreasingRun, "Must never have a decrease in value for digits")

let validate(rules: ValidationRule list)(password: Password) =
    rules |> List.fold(fun acc rule -> Result.bind rule acc)(Ok password)

let collectOkResults(results: Result<'a, 'b> list): 'a list =
    results
    |> List.collect(fun r -> 
        match r with
        | Ok(good) -> [good]
        | _ -> []
        )

let countValidPasswordAgainstRules(rules: ValidationRule list): int =
    seq { for i in 138307 .. 654504 -> Password.create(i) }
    |> Seq.toList
    |> List.map(validate(rules))
    |> collectOkResults
    |> List.length

let computeDay4Part1Answer(): int = 
    countValidPasswordAgainstRules([validateLength; hasAdjacentDigits; neverDecreases])

let private hasExactly2LongChainOfDigits(digits: PasswordDigit list): bool =
    digits
    |> List.groupBy(fun x -> x)
    |> List.map(fun (_, list) -> list.Length)
    |> List.contains(2)

let exactly2TheSame = validator(hasExactly2LongChainOfDigits, "Must have at least exactly one 2 long chain of repeating digits")

let computeDay4Part2Answer(): int = 
    countValidPasswordAgainstRules([validateLength; exactly2TheSame; neverDecreases])
