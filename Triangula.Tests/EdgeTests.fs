namespace Triangula.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Definitions
open Pos

[<TestClass>]
type EdgeTests() = 

    [<TestMethod>]
    member x.TestSortPos () = 
        let p1 = Pos.create 1 1
        let p2 = Pos.create 1 2
        let p3 = Pos.create 2 1
        let p4 = Pos.create 2 2
        let sorted = [| p4; p2; p1; p3 |] |> Array.sort
        Assert.AreEqual(sorted.[0], p1)
        Assert.AreEqual(sorted.[1], p2)
        Assert.AreEqual(sorted.[2], p3)
        Assert.AreEqual(sorted.[3], p4)
        Assert.IsTrue(p1 < p2)
        Assert.IsTrue(p1 < p3)
        Assert.IsTrue(p1 < p4)
        Assert.IsTrue(p2 < p3)
        Assert.IsTrue(p3 < p4)

    [<TestMethod>]
    member x.TestEdgesIntersect () = 
        let e1 = Edge.create (1, 1) (1, 2)
        let e2 = Edge.create (2, 1) (2, 2)
        let e3 = Edge.create (1, 1) (3, 2)
        Assert.IsFalse(Edge.intersect e1 e3)
        Assert.IsTrue(Edge.intersect e2 e3)
