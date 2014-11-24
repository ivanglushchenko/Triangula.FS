module Triangle

open Definitions
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

// Gets a "canonical" representation of a triangle. The idea: given 3 points A,B,C you
// can define a triangle in 6 different ways: ABC, ACB,  CBA, and so on. This function
// returns triangle ABC for any of the combinations of points.
let create p1 p2 p3 p =
    let sorted = [| p1; p2; p3 |] |> Array.sort
    { P1 = sorted.[0]; P2 = sorted.[1]; P3 = sorted.[2]; Player = p }