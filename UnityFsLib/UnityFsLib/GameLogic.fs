namespace UnityFsLib

open System
open System.Collections.Generic
open System.Numerics
open UnityEngine.UIElements.Experimental
open UnityFsLib.EntityComponent

module GameLogic =

    type GridPosition = GridPosition of int * int
    type Position = Position of Vector2
    type Velocity = Velocity of Vector2
    type Radian = Radian of float32

    [<Struct>]
    type World =
        { GridSize: float32
          MouseClicked: Component<Vector2 option> list
          TargetPositions: Component<Position> list
          Directions: Component<Radian> list
          Speeds: Component<float32> list
          Velocities: Component<Velocity> list
          CurrentPositions: Component<Position> list
          Grids: Component<Map<GridPosition, int>> list }

    let NewWorld =
        { GridSize = 10f
          MouseClicked = List.empty
          TargetPositions = List.empty
          Directions = List.empty
          Speeds = List.empty
          Velocities = List.empty
          CurrentPositions = List.empty
          Grids = List.empty }

    let addTuple a b =
        let a1, a2 = a
        let b1, b2 = b
        a1 + b1, a2 + b2


    let toGridPosition (Position position) gridSize =
        let gx = position.X / gridSize
        let gy = position.Y / gridSize
        GridPosition(int gx, int gy)

    let fromGridPosition (GridPosition(gx, gy)) gridSize =
        let x = gridSize * float32 gx
        let y = gridSize * float32 gy
        Vector2(x, y) |> Position

    let calcTarget position (grid: Map<_, _>) gridSize =
        let (GridPosition(gx, gy)) = toGridPosition position gridSize

        let dxyz =
            [| (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1) |]

        let targetGridPos, _ =
            dxyz
            |> Seq.map (fun (dx, dy) ->
                let gridPos = GridPosition(gx + dx, gy + dy)
                gridPos, grid.GetValueOrDefault(gridPos, 0))
            |> Seq.minBy (fun (_, c) -> c)

        fromGridPosition targetGridPos gridSize

    let calcDirection (Position current) (Position target) =
        let diff = target - current
        atan2 diff.Y diff.X |> Radian

    let calcVelocity (Radian direction) speed =
        let vx = speed * cos direction
        let vy = speed * sin direction
        Vector2(vx, vy) |> Velocity



    let calcGrid gridSize (positions: Position seq) =

        positions
        |> Seq.groupBy (fun p -> toGridPosition p gridSize)
        |> Seq.map (fun (k, values) -> k, Seq.length values)
        |> Map.ofSeq



    let update world : World =


        let targetPosition =
            world.CurrentPositions
            |> Seq.map (fun p ->
                { EntityId = p.EntityId
                  Value = calcTarget p.Value world.Grids.Head.Value world.GridSize })

        let nextDirection =
            world.Directions
            |> withSameEntity3 targetPosition world.CurrentPositions
            |> Seq.map (fun dct ->
                { EntityId = dct.EntityId
                  Value = calcDirection dct.Value2 dct.Value3 })

        let nextVelocity =
            world.Velocities
            |> withSameEntity3 world.Speeds nextDirection
            |> Seq.map (fun c3 ->
                { EntityId = c3.EntityId
                  Value = calcVelocity c3.Value2 c3.Value3 })

        let nextPosition =
            world.CurrentPositions
            |> withSameEntity2 nextVelocity
            |> Seq.map (fun c2 ->
                let (Position pos) = c2.Value1
                let (Velocity vel) = c2.Value2

                { EntityId = c2.EntityId
                  Value = Position(pos + vel) })

        let nextGridInfo =
            nextPosition
            |> withEntitySelector2 (fun _ comps -> comps |> Seq.head) world.Grids
            |> Seq.map (fun c2 -> c2.Value1)
            |> calcGrid world.GridSize

        let nextGrid =
            [ { EntityId = EntityId(0)
                Value = nextGridInfo } ]

        { GridSize = world.GridSize
          TargetPositions = targetPosition |> List.ofSeq
          MouseClicked = world.MouseClicked
          Directions = nextDirection |> List.ofSeq
          Speeds = world.Speeds
          Velocities = nextVelocity |> List.ofSeq
          CurrentPositions = nextPosition |> List.ofSeq
          Grids = nextGrid }


