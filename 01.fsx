#load "common.fsx"
open System

let getCalibrationNumber (line: string) =
    let digits = line |> Seq.filter Char.IsDigit |> Seq.map Char.GetNumericValue
    $"{Seq.head digits}{Seq.last digits}" |> int

let sample1 =
    """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

let sample1Result = sample1.ByNewLine() |> Seq.sumBy getCalibrationNumber

let part1 = Files[1] |> Seq.sumBy getCalibrationNumber

// --- Part Two ---

let sample2 =
    """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight
7pqrstsixteen"""


let digitWords =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
    |> List.indexed
    |> List.map (fun (i, word) -> {| Text = word; Value = i + 1 |})

let getCalibrationNumber2 (line: string) =
    let digits =
        line
        |> Seq.indexed
        |> Seq.filter (snd >> System.Char.IsDigit)
        |> Seq.map (fun (i, c) -> {| Value = Char.GetNumericValue c |> int; Position = i |})
        |> Seq.toList

    // find words in the line
    let words =
        digitWords
        |> List.collect (fun word ->
            let matchingWords =
                [
                    {|Value=word.Value; Position =line.IndexOf word.Text|}
                    {|Value=word.Value; Position =line.LastIndexOf word.Text|}
                ]
                |> List.filter (fun word -> word.Position > -1)
                |> List.sortBy (fun word -> word.Position)
            matchingWords)
    let combined = digits @ words |> List.sortBy _.Position |> List.map _.Value
    let result = $"{List.head combined}{List.last combined}"
    printfn "%s" result
    result |> int

// let sample2Result = sample2.ByNewLine() |> Seq.sumBy getCalibrationNumber2
// printfn "Sample Part 2: %d" sample2Result
printfn "Part 2: %d" (Files[1] |> Seq.sumBy getCalibrationNumber2)
