module Problem92

open System
open System.Collections.Generic

let toDigits n =
    let rec toDigitList digits n =
        match n with
        | 0 -> digits
        | _ -> toDigitList ((n % 10) :: digits) (n / 10)
    toDigitList [] n

let nextNumber n =
    toDigits(n) |> Seq.sumBy(fun elem -> elem * elem)

//let rec calculateEnding (n, endings: Dictionary<int, int>) =
//    if n = 1 || n = 89 then n
//    elif endings.ContainsKey(n) then endings.[n]
//    else
//        let ending = calculateEnding(nextNumber(n), endings)
//        if n < 600 then
//            endings.Add(n, ending) 
//        ending

//let rec calculateEnding (n, endings: Dictionary<int, int>) =
//    n |> function
//    | n when n = 1 -> n
//    | n when n = 89 -> n
//    | n when endings.ContainsKey(n) -> endings.[n]
//    | _ -> 
//        let ending = calculateEnding(nextNumber(n), endings)
//        if n < 600 then endings.Add(n, ending) 
//        ending

let rec calculateEnding (n, endings: Dictionary<int, int>) =
    match n with
    | n when n = 1 -> n
    | n when n = 89 -> n
    | n when endings.ContainsKey(n) -> endings.[n]
    | _ -> 
        let ending = calculateEnding(nextNumber(n), endings)
        if n < 600 then endings.Add(n, ending) 
        ending

let solve92() =
    let endings = new Dictionary<int, int>()
    seq { 1..999999} 
        |> Seq.filter(fun item -> calculateEnding(item, endings) = 89)
        |> Seq.length
        |> printfn "%i"
    Console.ReadLine() |> ignore
    0