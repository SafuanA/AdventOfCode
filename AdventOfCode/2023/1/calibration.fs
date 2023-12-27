module calibration

open System
open System.IO

let inputTxt = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let demo = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

let readLines (filePath:string) =  
    use sr = new StreamReader (filePath)
    sr.ReadToEnd ()

let parseInput (input: string) =
    input.Split("\r\n")
    |> Array.map (fun line -> line.Trim().ToCharArray()) // |> Array.map (fun c -> int (c - '0')))


let firstElements li = 
    match li with
        | a when Char.IsDigit(a) -> true
        | _ -> false

let sumFirstNumerciHeadAndTail input = 
    let first = Array.find firstElements input |> (fun x -> int(x-'0'))
    let last = Array.findBack firstElements input |> (fun x -> int(x-'0'))
    let y = first * 10 + last
    y

let Total li =
    Array.map sumFirstNumerciHeadAndTail li |> Array.sum

parseInput demo |> Total

readLines inputTxt |> parseInput |> Total