module ShapeRasterization

open Genotype
open System
open System.Drawing
open System.Drawing.Imaging
open ImageComparison

type Graphics with
    member this.Draw(p) =
        use brush = new SolidBrush(p.Color)
        this.FillPolygon(brush, p.Points);

    member this.DrawMany(ps) =
        ps
        |> Seq.sortBy (fun p -> p.ZIndex)
        |> Seq.iter this.Draw

let blank (bmp : Bitmap) =
    new Bitmap(bmp.Width, bmp.Height, bmp.PixelFormat)

let render (w, h, f : PixelFormat) ps =
    use bmp = new Bitmap(w, h, f);
    Graphics.FromImage(bmp).DrawMany(ps)
    getBytes bmp