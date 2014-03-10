module ImageComparison

open System
open System.Drawing
open System.Drawing.Imaging

let getBytes (img : Bitmap) =
    // lock image data for reading
    let data = img.LockBits(Rectangle(Point.Empty, img.Size), ImageLockMode.ReadOnly, img.PixelFormat);
    // allocate array
    let bytes = Math.Abs(data.Stride) * img.Height
    let rgbValues : byte array = Array.zeroCreate bytes
    // populate
    System.Runtime.InteropServices.Marshal.Copy(data.Scan0, rgbValues, 0, bytes)
    // unlock image data
    img.UnlockBits(data)
    rgbValues

let getDiff a b =
    if a > b then a - b else b - a
    
let pixelDiff (a : byte array) (b : byte array) =
    Seq.zip a b
    |> Seq.map (fun t -> t ||> getDiff)
    |> Seq.map int
    |> Seq.sum