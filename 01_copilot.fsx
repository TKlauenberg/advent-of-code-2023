open System.Text.RegularExpressions
open System

let sample2 =
    """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""

type Word = { Text: string; Value: int }

type DigitOrWord = { Value: int; Position: int }

let digitWords =
    [ for i in 1..9 ->
          { Text =
              match i with
              | 1 -> "one"
              | 2 -> "two"
              | 3 -> "three"
              | 4 -> "four"
              | 5 -> "five"
              | 6 -> "six"
              | 7 -> "seven"
              | 8 -> "eight"
              | 9 -> "nine"
              | _ -> failwith "Invalid number"
            Value = i } ]

let getCalibrationNumber2 (line: string) =
    let digits =
        [ for i in 0 .. line.Length - 1 do
              if Char.IsDigit(line.[i]) then
                  yield
                      { Value = int (string line.[i])
                        Position = i } ]

    let words =
        [ for word in digitWords do
              let matches = Regex.Matches(line, word.Text)

              for m in matches do
                  yield
                      { Value = word.Value
                        Position = m.Index } ]

    let combined = List.sortBy (fun x -> x.Position) (digits @ words)
    let values = List.map (fun x -> x.Value) combined

    int (string values.[0] + string values.[values.Length - 1])

let sample2Result =
    sample2.Split('\n')
    |> Array.filter (fun line -> line <> "")
    |> Array.sumBy getCalibrationNumber2

printfn "Sample 2: %d" sample2Result
