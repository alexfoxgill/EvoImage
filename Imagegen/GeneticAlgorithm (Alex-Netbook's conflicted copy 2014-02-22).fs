module GeneticAlgorithm

open System
open System.Drawing
open ImageComparison
open ShapeRasterization

type Individual = Individual of Triangle list
type Population = Population of Individual list

let fitness original =
    let origBytes = getBytes original
    let fitness' genome =
        genome
        |> render (original.Width, original.Height, original.PixelFormat)
        |> pixelDiff origBytes
    fitness'

type Random with
    member this.Point(rect : Rectangle) =
        Point(this.Next(rect.Left, rect.Right), this.Next(rect.Top, rect.Bottom))
    member this.Color() =
        Color.FromArgb(this.Next(0x1000000))
    member this.Triangle(rect) =
        {
        A = this.Point(rect);
        B = this.Point(rect);
        C = this.Point(rect);
        Color = this.Color();
        ZIndex = this.Next(100)
        }
    member this.Individual(n, rect) =
        [for i in 0..n -> this.Triangle(rect)]
    member this.Probability(x) =
        this.NextDouble() <= x



type Mutation(rand : Random, rect, mutationRate) =
    let mutPoint p =
        if rand.Probability(mutationRate) then rand.Point(rect) else p
    let mutColor c =
        if rand.Probability(mutationRate) then rand.Color() else c
    let mutZIndex z =
        if rand.Probability(mutationRate) then rand.Next(100) else z
    let mutTriangle t =
        {
        t with
            A = mutPoint t.A;
            B = mutPoint t.B;
            C = mutPoint t.C;
            Color = mutColor t.Color;
            ZIndex = mutZIndex t.ZIndex
        }
    member this.Mutate(x) =
        x |> List.map mutTriangle



let (||>>) (a,b) f = f a, f b
let (<<||) f (a,b) = f a, f b

let slice n list =
    list |> Seq.take n, list |> Seq.skip n

type Crossover(rand : Random, crossoverRate) =
    let onePoint a b =
        let max = (a, b) ||>> List.length ||> Operators.max |> (-) 1
        let point = rand.Next (1, max)
        let (w,x),(y,z) = (a, b) ||>> (slice point) ||>> (<<||) Seq.toList 
        List.append w z, List.append y x

    member this.OnePoint(a, b) =
        if rand.Probability(crossoverRate) then onePoint a b else a, b



let rand = Random()
let crossover = Crossover(rand, 0.8)
let mutation = Mutation(rand, Rectangle(Point.Empty, Size(500, 600)), 0.1)

type Algorithm(rand : Random, mutation, crossover) =
    let assess population fitness =
        let assessed =
            Async.Parallel [for x in population -> async { return x, fitness x }]
            |> Async.RunSynchronously
        let total = assessed |> Seq.sumBy snd
        assessed
            |> Seq.map (fun (x,f) -> x, 1.0 - (f / total))
        
    member this.CreatePopulation rect n =
        [for i in 1..n -> rand.Individual(rect)]
    member this.Advance(population, fitness : 'a -> float) =
        let pick = rand.NextDouble()
        assess population fitness
        |> Seq.scan (fun acc (x,f) -> x, (snd acc + f)) (null, 0.0)
        |> Seq.skip 1
        |> Seq.find (fun (x,f) -> f < pick)
        |> fst