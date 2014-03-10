module Options

open System
open System.Drawing
open Util

type IndividualOptions = {
    Polygons : IntRange
    Points : IntRange
    Blue : IntRange
    Red : IntRange
    Green : IntRange
    Alpha : IntRange
}

type MutationOptions = {
    AddPolygon : Probability.T
    RemovePolygon : Probability.T
    AddPoint : Probability.T
    RemovePoint : Probability.T
    MovePoint : Probability.T
    ChangeColor : Probability.T
    ChangeZIndex : Probability.T
}

type CrossoverMethod =
| OnePoint

type CrossoverOptions = {
    Rate : Probability.T

    Method : CrossoverMethod
}

type AlgorithmOptions = {
    Rand : Random
    Rect : Rectangle
    Elite : int
    PopulationSize : int

    Individual : IndividualOptions
    Mutation : MutationOptions
    Crossover : CrossoverOptions
}