module Probability

type T = Probability of float

let create = function
    | x when 0.0 <= x && x <= 1.0 -> Probability x
    | _ -> failwith "Invalid probability"

let percent = float >> ((*) 0.01) >> create

let test (r : System.Random) (Probability x) =
    r.NextDouble () <= x