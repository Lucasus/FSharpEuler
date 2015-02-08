module Problem59

open System

let isValidCharacter n = 32 <= n && n <= 122 && n <> 35 && n <> 47

let rec cartesianProduct LL = 
    match LL with
    | [] -> Seq.singleton []
    | L::Ls -> seq {for x in L do for xs in cartesianProduct Ls -> x::xs}

let decode text key =
    seq { 0 .. List.length text - 1 }
    |> Seq.map(fun(i) -> List.nth key (i % List.length key) ^^^ List.nth text i)
    |> Seq.takeWhile isValidCharacter

let sameLengthAs first second =
    Seq.length first = Seq.length second

let solve59() =
    let text =
        IO.File.OpenText("cipher.txt").ReadToEnd().Split(',') 
        |> Seq.map Int32.Parse
        |> Seq.toList

    cartesianProduct [seq { 'a' .. 'z'};seq { 'a' .. 'z' };seq { 'a' .. 'z' }] 
        |> Seq.map (Seq.map int)          
        |> Seq.map Seq.toList
        |> Seq.map (decode text)                
        |> Seq.filter (sameLengthAs text)
        |> Seq.map Seq.sum
        |> Seq.iter(printfn "%i")
    Console.ReadLine() |> ignore
    0
