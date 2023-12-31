module wait
open System
open System.IO


let demo = @"Time:      7  15   30
Distance:  9  40  200"

let inputTxt = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let readLines (filePath:string) =  
    use sr = new StreamReader (filePath)
    sr.ReadToEnd ()

let readTxt = readLines inputTxt

let parseInputAsString (input: string) =
    input.Trim().Split("\n")
    |> List.ofSeq

let inputSeq =  parseInputAsString readTxt |> List.map List.ofSeq 

let isDigit c =
    match c with
    | c when Char.IsDigit(c) -> true
    | _ -> false 

let rec crawlNumbers input = 
    match input with
    | [] -> []
    | c :: t  when Char.IsDigit(c) ->
        let numberStr = input |> List.takeWhile isDigit 
        let number = numberStr |> String.Concat |> int
        let rest =  input |> List.skip (numberStr |> Seq.length)
        number :: crawlNumbers rest
    | c :: t -> crawlNumbers t

let duration = inputSeq |> List.head |> crawlNumbers

let durationVector = duration |> List.map (fun x -> [0..x])

let possibleDistance = durationVector |> List.map (fun x -> 
        x |> List.map (fun y -> (List.max x - y) * y) 
    )

let threshholdDistance = inputSeq |> List.tail |> List.collect crawlNumbers

let filterThreshhold (possibleDistance:int List List) (threshholdDistance:int List) =
    List.map2 (fun distanceList threshold ->
        List.filter (fun distance -> distance > threshold) distanceList
    ) possibleDistance threshholdDistance

let totalDistance  = filterThreshhold possibleDistance threshholdDistance

let magicNumberSolution = totalDistance|> List.map List.length |> List.fold (*) 1