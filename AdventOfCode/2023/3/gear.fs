module gear
open System
open System.IO

let inputTxt = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let readLines (filePath:string) =  
    use sr = new StreamReader (filePath)
    sr.ReadToEnd ()


let parseInputAsString (input: string) =
    input.Split("\n")
    |> Array.map (fun line -> line.Trim())
    |> List.ofSeq

let demo = readLines inputTxt

type Symbol =
    | Empty of char
    | Digit of string
    | Any of char

//type Location = (x of int, y of int)
let mapToSymbol c =
    match c with 
    | c when System.Char.IsDigit(c) -> Digit (c |> string)
    | '.' -> Empty c
    | _ -> Any c

let parsed = 
    [for (i,row) in demo |> parseInputAsString |> List.indexed do
        [for (j, char) in row |> Seq.indexed -> (i, j), char |> mapToSymbol]]

type Bundle = 
    | Number of (int)
    | Empty of char
    | Any of char

let rec bundle line = 
    match line with
    | [] -> []
    | (_, Digit _ ) :: t ->
        let bundled =  
                line |> List.takeWhile (fun (loc, c) -> 
                    match c with
                    | Digit _ -> true
                    | _ -> false
                )
        let number = bundled |> List.map (snd >> (fun (Digit d) -> d)) |> String.concat "" |> int |> Number
        let location = bundled |> List.map (fst)
        let rest =  line |> List.skip (bundled |> Seq.length )
        (location, number) :: (bundle rest)
    | (loc,otherwise) :: t ->
        let entry =
            match otherwise with
                | Symbol.Empty otherwise -> ([loc], Bundle.Empty otherwise)
                | Symbol.Any otherwise -> ([loc], Bundle.Any otherwise)
        entry::bundle t

let bundled =  parsed |> List.collect bundle

let symbols =  bundled |> List.choose (fun (loc, d) ->
    match d with
        | Any _ -> Some(loc,d)
        | _ -> None
    )

let symbolLocs = symbols |> List.collect fst
    
let numbers = bundled |> List.choose (fun (loc, d) ->
    match d with
        | Number d -> Some(loc,d)
        | _ -> None
    )

let intersecs locations symbolLoc = 
    let x,y = symbolLoc
    let neighbs = [(x,y); (x-1,y); (x+1,y); (x,y-1); (x-1,y-1); (x+1,y-1); (x,y+1); (x-1,y+1); (x+1,y+1)]
    (Set.intersect (Set.ofList neighbs) (Set.ofList(locations))) |> Set.isEmpty |> not
   
let hasIntersec symbolLoc (locations, _) = 
    locations |> List.exists (intersecs symbolLoc)

let filter =
    numbers |> List.filter (hasIntersec symbolLocs)

filter |> List.sumBy snd