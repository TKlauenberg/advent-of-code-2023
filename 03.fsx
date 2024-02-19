#load "common.fsx"

open System
open System.Text.RegularExpressions

let sample1 =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

type PartNumberPosition = { StartX: int; EndX: int; Y: int }

type PartNumber =
    { Value: int
      Position: PartNumberPosition }

type SymbolPosition = { X: int; Y: int }

type Symbol =
    { Value: string
      Position: SymbolPosition }

let isAdjacent (partNumber: PartNumberPosition) (symbol: SymbolPosition) =
    let left = abs (partNumber.StartX - symbol.X) <= 1
    let right = abs (partNumber.EndX - symbol.X) <= 1
    let vertical = abs (partNumber.Y - symbol.Y) <= 1
    // printfn "left: %b, right: %b, vertical: %b" left right vertical
    // // print positions
    // printfn "partNumber: StartX: %d, EndX: %d, Y: %d" partNumber.StartX partNumber.EndX partNumber.Y
    // printfn "symbol: X: %d, Y: %d" symbol.X symbol.Y
    // printfn "result: %b" ((left || right) && vertical)
    (left || right) && vertical

let (|Number|_|) (value: string) =
    match System.Int32.TryParse value with
    | true, i -> Some i
    | _ -> None

// Symbol is a single character
let (|Symbol|_|) (value: string) =
    match value with
    | Number _ -> None
    | _ ->
        match value.Length with
        | 1 -> Some value
        | _ -> None

type PartNumbersAndSymbols =
    { PartNumbers: PartNumber seq
      Symbols: Symbol seq }

let processSchematic (schematic: string array) =
    let values =
        schematic
        |> Seq.mapi (fun y line ->
            Regex.Matches(line, @"\d+|[^.]")
            |> Seq.map (fun m ->
                {| Point = { X = m.Index; Y = y }
                   Value = m.Value |}))
        |> Seq.concat

    let partNumbers: PartNumber seq =
        values
        |> Seq.choose (fun m ->
            match m.Value with
            | Number i ->
                Some
                    { Value = i
                      Position =
                        { StartX = m.Point.X
                          EndX = m.Point.X + m.Value.Length - 1
                          Y = m.Point.Y } }
            | _ -> None)

    let symbols =
        values
        |> Seq.choose (fun m ->
            match m.Value with
            | Symbol s -> Some { Value = s; Position = m.Point }
            | _ -> None)
    // return record of PartNumber AND Symbols
    { PartNumbers = partNumbers; Symbols = symbols }

let getAllPartNumbers partNumbersAndSymbols=
    // return part Numbers which are Adjacent to any symbol
    partNumbersAndSymbols.PartNumbers
    |> Seq.filter (fun partNumber ->
        partNumbersAndSymbols.Symbols
        |> Seq.map _.Position
        |> Seq.exists (isAdjacent partNumber.Position)
        )


// let sample1Result =
//     sample1.ByNewLine()
//     |> processSchematic
//     |> Seq.map (fun partNumber ->
//         printfn "partnumber: %d" partNumber.Value
//         partNumber)
//     |> Seq.sumBy (fun partNumber -> partNumber.Value)

// printfn "Sample 1 Result: %d" sample1Result

// let answer1Result =
//     Files[3]
//     |> processSchematic
//     |> getAllPartNumbers
//     //print all values
//     |> Seq.map (fun partNumber ->
//         printfn "%d" partNumber.Value
//         partNumber)
//     |> Seq.sumBy (fun partNumber -> partNumber.Value)

// printfn "Answer Part 1: %d" answer1Result

// Part 2
let filterGears symbols =
    symbols
    |> Seq.filter (fun symbol -> symbol.Value = "*")

let getConnectedPartNumbers (partNumbers: PartNumber seq) symbol =
    let isSymbolAdjacent symbol (partNumber: PartNumber) = isAdjacent partNumber.Position symbol.Position

    partNumbers
    |> Seq.filter (isSymbolAdjacent symbol)
let calculateGears partNumbers symbols =
    symbols
    |> Seq.map (getConnectedPartNumbers partNumbers)
    |> Seq.filter (fun partNumbers -> Seq.length partNumbers = 2)
    |> Seq.map (Seq.map (fun partNumber -> partNumber.Value))
    |> Seq.sumBy (Seq.fold (*) 1)

let resultSample1 =
    sample1.ByNewLine()
    |> processSchematic
    |> fun partNumbersAndSymbols ->
        partNumbersAndSymbols.Symbols
        |> filterGears
        |> calculateGears partNumbersAndSymbols.PartNumbers
printfn "Sample 1 Result: %d" resultSample1

let resultAnswer2 =
    Files[3]
    |> processSchematic
    |> fun partNumbersAndSymbols ->
        partNumbersAndSymbols.Symbols
        |> filterGears
        |> calculateGears partNumbersAndSymbols.PartNumbers
printfn "Answer Part 2: %d" resultAnswer2