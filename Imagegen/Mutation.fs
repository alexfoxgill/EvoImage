module Mutation

open System.Drawing
open Options
open Genotype
open Util

let mutateColor opt c =
    if Probability.test opt.Rand opt.Mutation.ChangeColor
        then randomColor opt
        else c

let mutatePoint opt p =
    if Probability.test opt.Rand opt.Mutation.MovePoint
        then randomPoint opt
        else p

let mutateZIndex opt z =
    if Probability.test opt.Rand opt.Mutation.ChangeZIndex
        then opt.Rand.NextDouble()
        else z

let mutatePoints opt ps =
    let len = Array.length ps
    let mutateRemove ps =
        if len > opt.Individual.Points.Min && Probability.test opt.Rand opt.Mutation.RemovePoint
            then
                let i = opt.Rand.Next(len - 1)
                Seq.removeAt i ps
            else ps

    let mutateAdd ps =
        if len < opt.Individual.Points.Max && Probability.test opt.Rand opt.Mutation.AddPoint
            then Seq.add (randomPoint opt) ps
            else ps
    ps
    |> Array.map (mutatePoint opt)
    |> mutateRemove
    |> mutateAdd
    |> Array.ofSeq

let mutatePolygon opt p =
    {
        Points = mutatePoints opt p.Points
        Color = mutateColor opt p.Color
        ZIndex = mutateZIndex opt p.ZIndex
    }

let mutateIndividual opt (Individual ps) =
    let len = List.length ps
    let mutateRemove ps =
        if len > opt.Individual.Polygons.Min && Probability.test opt.Rand opt.Mutation.RemovePolygon
            then
                let i = opt.Rand.Next(len - 1)
                List.removeAt i ps
            else ps
    let mutateAdd ps =
        if len < opt.Individual.Polygons.Max && Probability.test opt.Rand opt.Mutation.AddPolygon
            then randomPolygon opt :: ps
            else ps
    ps
    |> List.map (mutatePolygon opt)
    |> mutateRemove
    |> mutateAdd
    |> Individual