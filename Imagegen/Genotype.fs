module Genotype

open System.Drawing
open Options
open Util

type Polygon = {
    Points: Point array;
    Color: Color;
    ZIndex : float;
}

type Individual = Individual of Polygon list

let randomPoint opt =
    Point(opt.Rand.Next(opt.Rect.Left, opt.Rect.Right), opt.Rand.Next(opt.Rect.Top, opt.Rect.Bottom))

let randomColor opt =
    (opt.Individual.Alpha, opt.Individual.Red, opt.Individual.Green, opt.Individual.Blue)
    |> Tuple4.map opt.Rand.Range
    |> Color.FromArgb

let randomPolygon opt =
    let numPoints = opt.Rand.Range opt.Individual.Points
    {
        Points = [| for _ in 1..numPoints do yield randomPoint opt |]
        Color = randomColor opt
        ZIndex = opt.Rand.NextDouble()
    }

let randomIndividual opt =
    let size = opt.Rand.Range opt.Individual.Polygons
    Individual [ for _ in 1..size do yield randomPolygon opt]