module seed
open System
open System.IO


let demo = @"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
1 0 69
0 69 1

humidity-to-location map:
60 56 37
56 93 4"

let inputTxt = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let readLines (filePath:string) =  
    use sr = new StreamReader (filePath)
    sr.ReadToEnd ()

let readTxt = readLines inputTxt

let parseInputAsString (input: string) =
    //input.Split("\n\n")
    input.Split ("\r\n\r\n")
    |> Array.map (fun line -> line.Trim())
    |> List.ofSeq

let isDigit c =
    match c with
    | c when Char.IsDigit(c) -> true
    | _ -> false 

let rec crawlNumbers input = 
    match input with
    | [] -> []
    | c :: t  when Char.IsDigit(c) ->
        let numberStr = input |> List.takeWhile isDigit 
        let number = numberStr |> String.Concat |> float
        let rest =  input |> List.skip (numberStr |> Seq.length)
        number :: crawlNumbers rest
    | c :: t -> crawlNumbers t

let inputSeq =  parseInputAsString readTxt|> List.map List.ofSeq 

let seeds = inputSeq |> List.head |> crawlNumbers

let maps = inputSeq |> List.tail |> List.map crawlNumbers |> List.map (List.chunkBySize 3)

let seedConvert singleMap seeds:float list =
    seeds 
    |> List.map (fun seed -> 
        let (_, finalSeed) = 
            singleMap 
            |> List.fold (fun (isTransformed, acc) src2tgt -> 
                let srcTgt = src2tgt |> List.head
                let srcSrc = src2tgt |> List.tail |> List.head
                let srcRange = src2tgt |> List.last
                if isTransformed && srcSrc <= acc && acc < srcSrc + srcRange then
                    let diff = srcTgt - srcSrc
                    (false, acc + float diff)
                else
                    (isTransformed, acc)
            ) (true, float seed)
        finalSeed
    )

let convertedSeed = maps |> List.fold (fun acc singleMap -> seedConvert singleMap acc) seeds |> List.min
