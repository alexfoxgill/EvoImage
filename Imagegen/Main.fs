module Main

open System
open System.Drawing
open System.Drawing.Imaging
open Genotype
open Options
open Util
open Mutation
open Crossover
open ShapeRasterization
open ImageComparison
open Selection

let compareTo original =
    let origBytes = getBytes original
    let max = Array.length origBytes * 255
    let monitor = new Object()
    let fitness (Individual genome) =
        let bytes = lock monitor (fun () -> render (original.Width, original.Height, original.PixelFormat) genome)
        pixelDiff origBytes bytes
    fun x -> Math.Pow(100.0, float (max - fitness x) / float max)

let createInitial opt =
    [ for i in 1..opt.PopulationSize do yield randomIndividual opt ]


[<EntryPoint>]
let main args =
    use original = new Bitmap(@"C:\Users\Alex.Gill\Dropbox\monalisa.png")

    let opt = {
        Rand = Random()
        PopulationSize = 10
        Rect = Rectangle(Point.Empty, original.Size)
        Elite = 2

        Individual =
            {
                Alpha = intRange 30 60
                Red = intRange 0 255
                Green = intRange 0 255
                Blue = intRange 0 255
                Points = intRange 3 7
                Polygons = intRange 10 150
            }
        Mutation =
            {
                AddPoint = Probability.create 0.006
                RemovePoint = Probability.create 0.006
                MovePoint = Probability.create 0.006
                AddPolygon = Probability.create 0.006
                RemovePolygon = Probability.create 0.006
                ChangeZIndex = Probability.create 0.012
                ChangeColor = Probability.create 0.006
            }
        Crossover =
            {
                Rate = Probability.percent 95
                Method = OnePoint
            }
    }

    let fitness = compareTo original
    let mutable population = createInitial opt |> Seq.ofList

    
    let save (Individual ps) f =
        use output = blank original
        Graphics.FromImage(output).DrawMany(ps)
        let filename = @"C:\Users\Alex.Gill\Dropbox\monalisa\" + f.ToString() + ".png"
        output.Save(filename)
        
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    for gen in 1..99999999 do
        let pop, (x,f) = advance opt fitness population
        printfn "Generation: %d | Time: %A | Fittest individual: %f" gen stopwatch.Elapsed f
        if gen % 1000 = 1 then do
            save x f

        population <- pop

    0