module Selection

open Util
open Options
open Genotype
open Mutation
open Crossover

let assessAsync fitness population = 
    seq { for x in population do yield async { return x, fitness x } }
    |> Async.Parallel
    |> Async.RunSynchronously

let assess fitness population = Seq.map (fun x -> x, fitness x) population

let seqSelect opt assessed =
    Seq.initInfinite (fun _ -> opt.Rand.ChooseWeighted assessed)

let newIndividuals opt selectStream =
    selectStream
    |> Seq.pairwise
    |> Seq.everyNth 2
    |> Seq.map (fun (a,b) -> crossover opt a b)
    |> Seq.untuple
    |> Seq.map (mutateIndividual opt)

let elite opt assessed =
    assessed
    |> Seq.sortBy (fun (x,f) -> -f)
    |> Seq.map fst
    |> Seq.take opt.Elite

let advance opt fitness population =
    let assessed = assessAsync fitness population
    let advanced =
        assessed
        |> seqSelect opt
        |> newIndividuals opt
        |> Seq.append (elite opt assessed)
        |> Seq.take opt.PopulationSize
    advanced, Seq.maxBy snd assessed