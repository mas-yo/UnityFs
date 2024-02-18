module Tests

open System
open System.Numerics
open Xunit
open UnityFsLib.GameLogic

[<Fact>]
let TestCalcWaitingTime () =
    let (WaitingTime result) = calcWaitingTime (WaitingTime 10) (Quickness 10)
    Assert.Equal(9, result)
    let (WaitingTime result) = calcWaitingTime (WaitingTime 0) (Quickness 10)
    Assert.Equal(10, result)

[<Theory>]
[<InlineData(0, 1, 0, 1)>]
[<InlineData(0, 0, 0, 0)>]
let TestCalcTargetWhenAlone currentTgtX currentTgtY expectedTgtX expectedTgtY =
    let grid =
        [| (GridPosition(0, 0), 1); (GridPosition(0, 1), 0) |]
        |> Map.ofArray
        |> Grid
        
    let currentPosition = Position (Vector2(0.1f, 0.1f))
    let currentTarget = GridPosition (currentTgtX, currentTgtY)
    let (GridPosition (gx, gy)) = calcTarget 1f currentTarget currentPosition grid
    Assert.Equal((expectedTgtX, expectedTgtY), (gx, gy))

[<Theory>]
[<InlineData(0, 1, 0, 1)>]
[<InlineData(0, 0, 0, 1)>]
let TestCalcTargetWhenOtherExists currentTgtX currentTgtY expectedTgtX expectedTgtY =
    let grid =
        [| (GridPosition(0, 0), 2); (GridPosition(0, 1), 0) |]
        |> Map.ofArray
        |> Grid
        
    let currentPosition = Position (Vector2(0.1f, 0.1f))
    let currentTarget = GridPosition (currentTgtX, currentTgtY)
    let (GridPosition (gx, gy)) = calcTarget 1f currentTarget currentPosition grid
    Assert.Equal((expectedTgtX, expectedTgtY), (gx, gy))

[<Theory>]
[<InlineData(0, 0, 1, 0)>]
let TestCalcTargetChooseNearest currentTgtX currentTgtY expectedTgtX expectedTgtY =
    let grid =
        [| (GridPosition(0, 0), 2); (GridPosition(0, 1), 1); (GridPosition(1, 0), 0); (GridPosition(1, 1), 0) |]
        |> Map.ofArray
        |> Grid
        
    let currentPosition = Position (Vector2(0.1f, 0.1f))
    let currentTarget = GridPosition (currentTgtX, currentTgtY)
    let (GridPosition (gx, gy)) = calcTarget 1f currentTarget currentPosition grid
    Assert.Equal((expectedTgtX, expectedTgtY), (gx, gy))
