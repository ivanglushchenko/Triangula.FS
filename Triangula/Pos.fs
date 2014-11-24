module Pos

type Pos = int * int

let create xVal yVal = (xVal, yVal)

let x = function (x, _) -> x

let y = function (_, y) -> y

let diff p1 p2 = (x p2 - x p1, y p2 - y p1)

let dx p1 p2 = x p1 - x p2

let dy p1 p2 = y p1 - y p2

let length p1 p2 = (dx p1 p2) * (dx p1 p2) + (dy p1 p2) * (dy p1 p2) |> float |> sqrt

let dot p1 p2 = x p1 * x p2 + y p1 * y p2 |> float
