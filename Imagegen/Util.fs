module Util

module Tuple2 =
    let map f (a,b) = f a, f b

module Tuple3 =
    let map f (a,b,c) = f a, f b, f c

module Tuple4 =
    let map f (a,b,c,d) = f a, f b, f c, f d


module Seq =
    let enumerate xs = Seq.zip (Seq.initInfinite id) xs
    let removeAt i xs = seq { for n,x in enumerate xs do if n <> i then yield x }
    let everyNth n xs = seq { for i,x in enumerate xs do if i % n = 0 then yield x }
    let scan0 f xs = Seq.scan f (Seq.head xs) (Seq.skip 1 xs)
    let add y xs =
        seq {
            for x in xs do
                yield x
            yield y }
    let untuple xs =
        seq {
            for (a,b) in xs do
                yield a
                yield b }
    let slice i xs =
        xs |> Seq.take i, xs |> Seq.skip i

module List =
    let rec removeAt i list =
        match list with
        | x::xs when i = 0 -> xs
        | x::xs -> x :: removeAt (i - 1) xs
        | [] -> []

type IntRange = {
    Max : int;
    Min : int;
}

let intRange min max = { Min = min; Max = max }

type System.Random with
    member this.Range(r) =
        this.Next(r.Min, r.Max)
    member this.ChooseWeighted xs =
        let pick = this.NextDouble() * Seq.sumBy snd xs
        let x = Seq.head xs |> fst
        xs
        |> Seq.scan (fun (_,acc) (x,f) -> x, acc - f) (x, pick)
        |> Seq.find (fun (x,f) -> f < 0.0)
        |> fst
