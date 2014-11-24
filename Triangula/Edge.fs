module Edge

open Pos

type Edge = { From: Pos; To: Pos }

let create pFrom pTo = { From = pFrom; To = pTo }

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
