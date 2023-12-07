#load "common.fsx"
open System

let sample1 =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""

// process a round with a value like 6 red, 1 blue, 3 green
let processRound (round: string) =
    let splits = round.Split [| ',' |]

    splits
    |> Seq.map (fun color -> color.Split [| ' ' |])
    |> Seq.map (fun color ->
        {| Color = color.[1]
           Count = int color.[0] |})
    |> Seq.toList


let processLine (line: string) =
    let splitFirst = line.Split [| ':' |]
    let gameId = splitFirst.[0].Substring(4) |> int
    let rounds = splitFirst.[1].Split [| ';' |] |> Seq.map processRound |> Seq.toList
    {| GameId = gameId; Rounds = rounds |}

let isGamePossible (bag: {| Color: string; Count: int |} list) (game: {| GameId: int; Rounds: {| Color: string; Count: int |} list list |}) =
    game.Rounds
        |> Seq.forall (fun round ->
            round
            |> Seq.forall (fun (color: {| Color: string; Count: int |}) ->
                color.Count <= (bag |> Seq.find (fun c -> c.Color = color.Color) |> _.Count)))

let bag =
    [ {| Color = "red"; Count = 12 |}
      {| Color = "green"; Count = 13 |}
      {| Color = "blue"; Count = 14 |} ]

let isThisGamePossible = isGamePossible bag

let sample1Result =
    sample1.ByNewLine()
    |> Seq.map processLine
    |> Seq.filter isThisGamePossible
    |> Seq.sumBy _.GameId

printfn "Part 1 sample: %d" sample1Result

let part1Result =
    Files[2]
    |> Seq.map processLine
    |> Seq.filter isThisGamePossible
    |> Seq.sumBy _.GameId

printfn "Part 1: %d" part1Result

// --- Part Two ---

let sample2 = sample1

let colorsToMap (colors: {| Color: string; Count: int |} list) =
    colors
    |> Seq.map (fun color -> color.Color, color.Count)
    |> Map.ofSeq

let minimalAmountNeeded (game: {| GameId: int; Rounds: {| Color: string; Count: int |} list list |}) =
    game.Rounds
        |> Seq.map colorsToMap
        |> Seq.reduce (fun acc cur ->
            cur
            |> Map.fold (fun state key value ->
                match Map.tryFind key state with
                | Some value' when value' >= value -> Map.add key value' state
                | Some _ -> Map.add key value state
                | None -> Map.add key value state) acc )

let sample2Result =
    sample2.ByNewLine()
    |> Seq.map processLine
    |> Seq.map minimalAmountNeeded
    |> Seq.map (Map.fold (fun state key value -> state * value) 1)
    |> Seq.sum
printfn "Part 2 sample: %d" sample2Result

let part2Result =
    Files[2]
    |> Seq.map processLine
    |> Seq.map minimalAmountNeeded
    |> Seq.map (Map.fold (fun state key value -> state * value) 1)
    |> Seq.sum
printfn "Part 2: %d" part2Result
