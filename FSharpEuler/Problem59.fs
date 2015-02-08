module Problem59

let isValidCharacter n = 32 <= n && n <= 122 && n <> 35 && n <> 47

let rec cartesianProduct LL = 
    match LL with
    | [] -> Seq.singleton []
    | L::Ls -> seq {for x in L do for xs in cartesianProduct Ls -> x::xs}

let decode (text: list<int>) (key: list<int>) =
    seq { 0..text.Length - 1}
    |> Seq.map(fun(i) -> key.[i % key.Length] ^^^ text.[i])
    |> Seq.takeWhile isValidCharacter
    |> Seq.toList

let sameLengthAs<'T> (first: list<'T>) (second: list<'T>) =
    first.Length = second.Length

let solve59() =
    let text =
        System.IO.File.OpenText("cipher.txt").ReadToEnd().Split(',') 
        |> Seq.map System.Int32.Parse
        |> Seq.toList

    cartesianProduct [seq { 'a' .. 'z'};seq { 'a' .. 'z' };seq { 'a' .. 'z' }] 
        |> Seq.map (Seq.map int)            
        |> Seq.map Seq.toList
        |> Seq.map (decode text)                
        |> Seq.filter (sameLengthAs text)
        |> Seq.map Seq.sum
        |> Seq.iter(printfn "%i")
    System.Console.ReadLine() |> ignore
    0
