module scratchcard
open System
open System.IO


let demo = @"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

let inputTxt = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let readLines (filePath:string) =  
    use sr = new StreamReader (filePath)
    sr.ReadToEnd ()


let parseInputAsString (input: string) =
    input.Split("\n")
    |> Array.map (fun line -> line.Trim())
    |> List.ofSeq

let splitter (symbol:char) (line:string) = 
    line.Split(symbol) |> List.ofArray

let parsed input = 
    [for (row) in input |> parseInputAsString do
        row.Split(':') |> Array.last |> splitter '|' ] //|> Array.map (splitter ';')]

let isDigit c =
    match c with
    | c when Char.IsDigit(c) -> true
    | _ -> false


let rec numbers input = 
    match input with
    | [] -> []
    | ' ' :: t -> numbers t
    | c :: t ->
        let numberStr = 
            input |> List.takeWhile isDigit
        let number = numberStr |> String.Concat |> int
        let rest =  input |> List.skip (numberStr |> Seq.length)

        number:: numbers rest

let winners input = 
    input |> parsed |> List.map (Seq.head >> Seq.toList >> numbers)

let draws input = 
    input |> parsed |> List.map (Seq.last >> Seq.toList >> numbers)

let rec intersec xl yl = 
  match xl, yl with 
   | [], _ | _, [] -> 0.0
   | x::xs, y::ys -> 
     let count = (Set.intersect (x |> Set.ofList) (y |> Set.ofList) |> Set.count) - 1
     let points =
        match count with
        |_ when count < 0 -> 0.0
        |_ -> 2.0 ** (count)
     let next = intersec xs ys
     points + next

let total input = 
    intersec (winners input) (draws input)

demo |> total 

readLines inputTxt |> total 