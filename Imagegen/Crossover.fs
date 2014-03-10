module Crossover

open Util
open Genotype
open Options

let onePoint opt a b =
    let max = ((a,b) |> Tuple2.map List.length ||> Operators.min) - 2
    let p = opt.Rand.Next(1, max)
    let (w,x),(y,z) = (a, b) |> Tuple2.map (Seq.slice p)
    (Seq.append w z, Seq.append y x) |> Tuple2.map Seq.toList

let crossover opt (Individual a) (Individual b) =
    if Probability.test opt.Rand opt.Crossover.Rate
        then
            match opt.Crossover.Method with
            | OnePoint -> onePoint opt a b
        else
            a, b
    |> Tuple2.map Individual