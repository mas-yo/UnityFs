namespace UnityFsLib

open System
open System.Collections.Generic
open System.Numerics
open UnityEngine.UIElements
open UnityEngine.UIElements.Experimental
open UnityFsLib.EntityComponent

module GameLogic =

    type GridPosition = GridPosition of int * int
    type Position = Position of Vector2
    type Velocity = Velocity of Vector2
    type Radian = Radian of float32
    type Grid = Grid of Map<GridPosition, int>
    
    type WaitingTime = WaitingTime of int
    

    [<Struct>]
    type World =
        { GridSize: float32
          WaitingTimes: Component<WaitingTime> list
          TargetPositions: Component<GridPosition> list
          Directions: Component<Radian> list
          Speeds: Component<float32> list
          Velocities: Component<Velocity> list
          CurrentPositions: Component<Position> list
          Grids: Component<Grid> list }

    let NewWorld =
        { GridSize = 1f
          WaitingTimes = [
              { EntityId = EntityId(1); Value = WaitingTime(120) }
              { EntityId = EntityId(2); Value = WaitingTime(80) }
              { EntityId = EntityId(3); Value = WaitingTime(20) }
          ] 
          TargetPositions = [
              { EntityId = EntityId(1); Value = GridPosition(0, 0) }
              { EntityId = EntityId(2); Value = GridPosition(0, 0) }
              { EntityId = EntityId(3); Value = GridPosition(0, 0) }
          ]
          Directions =  [
              { EntityId = EntityId(1); Value = Radian(0f) }
              { EntityId = EntityId(2); Value = Radian(0f) }
              { EntityId = EntityId(3); Value = Radian(0f) }
          ]
          Speeds = [
              { EntityId = EntityId(1); Value = 0.01f }
              { EntityId = EntityId(2); Value = 0.01f }
              { EntityId = EntityId(3); Value = 0.01f }
          ]
          Velocities = [
              { EntityId = EntityId(1); Value = Velocity(Vector2(0f, 0f)) }
              { EntityId = EntityId(2); Value = Velocity(Vector2(0f, 0f)) }
              { EntityId = EntityId(3); Value = Velocity(Vector2(0f, 0f)) }
          ]
          CurrentPositions = [
              { EntityId = EntityId(1); Value = Position(Vector2(0f, 0f)) }
              { EntityId = EntityId(2); Value = Position(Vector2(0.1f, 0f)) }
              { EntityId = EntityId(3); Value = Position(Vector2(0f, 1f)) }
          ]
          Grids = [
              { EntityId = EntityId(0); Value = Grid(Map.empty<_,_>) }
          ] }
        
    let toGridPosition (Position position) gridSize =
        let gx = position.X / gridSize
        let gy = position.Y / gridSize
        GridPosition(int gx, int gy)

    let fromGridPosition (GridPosition(gx, gy)) gridSize =
        let x = gridSize * float32 gx
        let y = gridSize * float32 gy
        Vector2(x, y) |> Position        
    let AddEntity entityId position world =
        {
            GridSize = world.GridSize
            WaitingTimes = { EntityId = entityId; Value = WaitingTime(60) } :: world.WaitingTimes
            TargetPositions = { EntityId = entityId; Value = (toGridPosition position world.GridSize) } :: world.TargetPositions
            Directions = { EntityId = entityId; Value = Radian(0f) } :: world.Directions
            Speeds = { EntityId = entityId; Value = 0.01f } :: world.Speeds
            Velocities = { EntityId = entityId; Value = Velocity(Vector2(0f, 0f)) } :: world.Velocities
            CurrentPositions = { EntityId = entityId; Value = position } :: world.CurrentPositions
            Grids = world.Grids
        }

    let addTuple a b =
        let a1, a2 = a
        let b1, b2 = b
        a1 + b1, a2 + b2

    let lengthSquared (Position p1) (Position p2) =
        (p1 - p2).LengthSquared()



    
    let calcWaitingTime gridSize (WaitingTime waitingTime) currentPos targetGridPos =
        let isInSameGrid gridSize pos gridPos  =
            gridPos = toGridPosition pos gridSize

        match waitingTime with
        | w when w = 0 -> 60
        // | w when w < 60 -> max 0 (w - 1)
        | w -> max 0 (w - 1)
            
        // if waitingTime > 0 && isInSameGrid gridSize currentPos targetGridPos 
        // then
        //     max 0 (waitingTime - 1)
        // else
        //     0
        |> WaitingTime
        
    let calcTarget gridSize targetGridPos position (WaitingTime waitingTime) (Grid grid)  =
        
        let searchGrid gridSize pos  =
            let (GridPosition(gx, gy)) = toGridPosition pos gridSize

            let dxyz =
                [| (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1) |]

            let targetGridPos, _ =
                dxyz
                    |> Seq.map (fun (dx, dy) ->
                        let gridPos = GridPosition(gx + dx, gy + dy)
                        gridPos, grid.GetValueOrDefault(gridPos, 0))
                    |> Seq.minBy (fun (_, c) -> c)                    
            targetGridPos
            
        let currentGrid = toGridPosition position gridSize
        let nearCount = grid.GetValueOrDefault(currentGrid, 0)
            
        if nearCount > 1 && waitingTime = 0
        then
            searchGrid gridSize position
        else
            targetGridPos
                
        
        

    let calcDirection gridSize (direction:Radian) (Position current) (targetGridPos: GridPosition) =
        let (Position p) = fromGridPosition targetGridPos gridSize
        let diff = p - current
        atan2 diff.Y diff.X |> Radian

    let calcVelocity (Velocity _) (Radian direction) speed =
        Vector2(speed * cos direction, speed * sin direction)
        |> Velocity

    let calcPosition gridSize (Position current) (Velocity velocity) (targetGridPos: GridPosition) =
        let (Position p) = fromGridPosition targetGridPos gridSize
        let l1 = (current - p).LengthSquared()
        let l2 = (current + velocity - p).LengthSquared()
        
        if l1 > l2
        then current + velocity
        else current
        |> Position

    let calcGrid gridSize (positions: Position seq) =

        positions
        |> Seq.groupBy (fun p -> toGridPosition p gridSize)
        |> Seq.map (fun (k, values) -> k, Seq.length values)
        |> Map.ofSeq
        |> Grid

    let Update (world: World) =

        let gridSelector (comps: Component<Grid> seq) (_: Component<_>) =
            comps |> Seq.head
            
        let nextWaitingTimes =
            world.WaitingTimes
            |> nextValueWithSameEntity3 (calcWaitingTime world.GridSize) world.CurrentPositions world.TargetPositions 
            |> List.ofSeq

        let nextTargets =
            world.TargetPositions
            |> withEntitySelector4
                   (sameEntitySelector world.CurrentPositions)
                   (sameEntitySelector nextWaitingTimes)
                   (gridSelector world.Grids)
            |> Seq.map (fun (entityId, target, position, waitingTime, grid) ->
                                    {
                                        EntityId = entityId
                                        Value = calcTarget world.GridSize target position waitingTime grid
                                    })
            |> List.ofSeq

        let nextDirection =
            world.Directions
            |> nextValueWithSameEntity3 (calcDirection world.GridSize) world.CurrentPositions nextTargets 
            |> List.ofSeq
            
        let nextVelocity =
            world.Velocities
            |> nextValueWithSameEntity3 calcVelocity nextDirection world.Speeds
            |> List.ofSeq

        let nextPosition =
            world.CurrentPositions
            |> nextValueWithSameEntity3 (calcPosition world.GridSize) nextVelocity nextTargets
            |> List.ofSeq

        let nextGridInfo =
            nextPosition
            |> Seq.map (fun p -> p.Value)
            |> (calcGrid world.GridSize)
            
            // |> withEntitySelector2 (gridSelector world.Grids)
            // |> Seq.map (fun (entityId, value1, value2) -> calcGrid world.GridSize value1 value2)
            // |> Grid

        let nextGrid =
            [ { EntityId = EntityId(0)
                Value = nextGridInfo } ]

        { GridSize = world.GridSize
          WaitingTimes = nextWaitingTimes 
          TargetPositions = nextTargets
          Directions = nextDirection
          Speeds = world.Speeds
          Velocities = nextVelocity
          CurrentPositions = nextPosition
          Grids = nextGrid }


