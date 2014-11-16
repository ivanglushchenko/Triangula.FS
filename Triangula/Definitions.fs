module Definitions

type Player = 
    | Player1
    | Player2

module Pos =
    type Pos = int * int

    let create xVal yVal = (xVal, yVal)

    let x = function (x, _) -> x

    let y = function (_, y) -> y

    let diff p1 p2 = (x p2 - x p1, y p2 - y p1)

    let dx p1 p2 = x p1 - x p2

    let dy p1 p2 = y p1 - y p2

    let length p1 p2 = (dx p1 p2) * (dx p1 p2) + (dy p1 p2) * (dy p1 p2) |> float |> sqrt

    let dot p1 p2 = x p1 * x p2 + y p1 * y p2 |> float


module Edge =
    open Pos

    type Edge = { From: Pos; To: Pos }

    let equal e1 e2 = 
        (e1.From = e2.From && e1.To = e2.To) || (e1.From = e2.To && e1.To = e2.From)

    // Checks if two edges intersect one another.
    let intersect e1 e2 =
        if equal e1 e2 then true
        else if 
            (e1.From = e2.From && e1.To <> e2.To) || 
            (e1.From <> e2.From && e1.To = e2.To) || 
            (e1.From = e2.To && e1.To <> e2.From) || 
            (e1.From <> e2.To && e1.To = e2.From) then false
        else if x e1.To <> x e1.From then
            let t2 = float ((dy e2.From e1.From) * (dx e1.To e1.From) - (dx e2.From e1.From) * (dy e1.To e1.From)) / float ((dx e2.To e2.From) * (dy e1.To e1.From) - (dy e2.To e2.From) * (dx e1.To e1.From))
            let t1 = (float (dx e2.To e2.From) * t2 + float(dx e2.From e1.From)) / float (dx e1.To e1.From)
            t2 >= 0.0 && t2 <= 1.0 && t1 >= 0.0 && t1 <= 1.0
        else
            let t2 = float ((dx e2.From e1.From) * (dy e1.To e1.From) - (dy e2.From e1.From) * (dx e1.To e1.From)) / float ((dy e2.To e2.From) * (dx e1.To e1.From) - (dx e2.To e2.From) * (dy e1.To e1.From))
            let t1 = (float (dy e2.To e2.From) * t2 + float (dy e2.From e1.From)) / float (dy e1.To e1.From)
            t2 >= 0.0 && t2 <= 1.0 && t1 >= 0.0 && t1 <= 1.0

    // Checks if the edge contains the given point.
    let contains p e =
        if e.From = p || e.To = p then false
        else 
            let (f1, f2, f3) = if x e.From <> x e.To then (dx, dy, y) else (dy, dx, x)
            let t = float (f1 p e.From) / float (f1 e.To e.From)
            if t > 1.0 || t < 0.0 then false
            else
                let t2 = float (f2 e.To e.From) * t + float (f3 e.From)
                abs (t2 - float(f3 p)) < 0.00001

    let reverse e = { From = e.To; To = e.From }


module Triangle =
    open Pos
    open Edge

    type Triangle = { P1 : Pos; P2: Pos; P3: Pos; Player: Player }

    let area t =
        let l1 = length t.P1 t.P2
        let l2 = length t.P2 t.P3
        let l3 = length t.P1 t.P3
        let s = (l1 + l2 + l3) / 2.0
        s * (s - l1) * (s - l2) * (s - l3) |> sqrt

    let contains p t =
        let v0 = diff t.P2 t.P1
        let v1 = diff t.P3 t.P1
        let v2 = diff p t.P1
        let dot00 = dot v0 v0
        let dot01 = dot v0 v1
        let dot02 = dot v0 v2
        let dot11 = dot v1 v1
        let dot12 = dot v1 v2
        let invDenom = 1.0 / (dot00 * dot11 - dot01 * dot01)
        let u = (dot11 * dot02 - dot01 * dot12) * invDenom
        let v = (dot00 * dot12 - dot01 * dot02) * invDenom
        (u > 0.0) && (v > 0.0) && (u + v < 1.0)

    let create p1 p2 p3 p =
        //let pMin = if p1 <= 
        { P1 = p1; P2 = p2; P3 = p3; Player = p }