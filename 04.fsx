#load "common.fsx"

open System

type Card =
    { Name: int
      WinningNumbers: int list
      NumbersYouHave: int list }

let parseCard (line: string) =
    let parts = line.Split([| ':' |], StringSplitOptions.RemoveEmptyEntries)
    let numbers = parts.[1].Split([| '|' |], StringSplitOptions.RemoveEmptyEntries)

    let cardNumbers =
        numbers.[0].Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    let pipeNumbers =
        numbers.[1].Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    { Name = parts.[0].Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries).[1]|>Int32.Parse
      WinningNumbers = cardNumbers |> Array.toList
      NumbersYouHave = pipeNumbers |> Array.toList }


let getWinningNumbers card =
    card.NumbersYouHave
    |> List.filter (fun n -> card.WinningNumbers |> List.contains n)

let calculatePointsOfCard card =
    let getLength = card |> getWinningNumbers |> List.length

    match getLength with
    | 0 -> 0
    | 1 -> 1
    | _ -> int (Math.Pow(float (2), float (getLength - 1)))


let sample1 =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""

// let resultSamplePart1 =
//     sample1.ByNewLine()
//     |> Array.map (parseCard >> calculatePointsOfCard)
//     // |> Array.map (fun x -> printfn "%A" x; x)
//     |> Array.sum
// printfn "Result sample part 1: %A" resultSamplePart1
// let resultPart1 =
//     Files.[4]
//     |> Array.map (parseCard >>calculatePointsOfCard)
//     |> Array.sum
// printfn "Result part 1: %A" resultPart1

// Part 2

let rec calculatePointsOfCardPart2 (cardPointMap: Map<int,int>) cardNumber =
    match cardPointMap.TryFind cardNumber with
    | Some points ->
        1 + (seq { cardNumber+1..cardNumber + points} |> Seq.map (calculatePointsOfCardPart2 cardPointMap) |> Seq.sum)
    | None -> 1
let calculatePointsPart2 cards =
    let getLength card = card |> getWinningNumbers |> List.length
    let pointMap = cards |> Array.map (fun card -> card.Name, card |> getLength) |> Map.ofArray
    // pointMap |> Map.iter (fun k v -> printfn "%A: %A" k v)
    cards
    |> Array.map (fun card -> calculatePointsOfCardPart2 pointMap card.Name)
    // |> Array.map (fun x -> printfn "%A" x; x)
    |> Array.sum
let resultSamplePart2 =
    sample1.ByNewLine()
    |> Array.map parseCard
    |> calculatePointsPart2
printfn "Result sample part 2: %A" resultSamplePart2

let resultPart2 =
    Files.[4]
    |> Array.map parseCard
    |> calculatePointsPart2
printfn "Result part 2: %A" resultPart2