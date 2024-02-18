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
    type Quickness = Quickness of int
    

    [<Struct>]
    type World =
        { GridSize: float32
          Quickness: Component<Quickness> list
          WaitingTimes: Component<WaitingTime> list
          TargetPositions: Component<GridPosition> list
          Directions: Component<Radian> list
          Speeds: Component<float32> list
          Velocities: Component<Velocity> list
          CurrentPositions: Component<Position> list
          Grids: Component<Grid> list }
        
        
    let NewGrids =
        [ -10 .. 10 ]
        |> List.collect (fun x -> [ -10 .. 10 ] |> List.map (fun y -> x, y))
        |> List.map (fun x -> (GridPosition(x), 0))
        |> Map.ofList
        
    let NewWorld =
        { GridSize = 1f
          Quickness = List.Empty
          WaitingTimes = List.Empty 
          TargetPositions = List.Empty
          Directions = List.Empty
          Speeds = List.Empty
          Velocities = List.Empty
          CurrentPositions = List.Empty
          Grids = [
              { EntityId = EntityId(0); Value = Grid(NewGrids) }
          ] }
        
    let toGridPosition gridSize (Position position) =
        let gx = position.X / gridSize
        let gy = position.Y / gridSize
        GridPosition(int gx, int gy)

    let fromGridPosition gridSize (GridPosition(gx, gy)) =
        let x = gridSize * float32 gx
        let y = gridSize * float32 gy
        Vector2(x, y) |> Position        
    let AddEntity entityId position speed quickness world =
        {
            GridSize = world.GridSize
            Quickness = { EntityId = entityId; Value = Quickness(quickness) } :: world.Quickness 
            WaitingTimes = { EntityId = entityId; Value = WaitingTime(quickness) } :: world.WaitingTimes
            TargetPositions = { EntityId = entityId; Value = (toGridPosition world.GridSize position) } :: world.TargetPositions
            Directions = { EntityId = entityId; Value = Radian(0f) } :: world.Directions
            Speeds = { EntityId = entityId; Value = speed } :: world.Speeds
            Velocities = { EntityId = entityId; Value = Velocity(Vector2(0f, 0f)) } :: world.Velocities
            CurrentPositions = { EntityId = entityId; Value = position } :: world.CurrentPositions
            Grids = world.Grids
        }

    let lengthSquared (Position p1) (Position p2) =
        (p1 - p2).LengthSquared()

    let calcWaitingTime (WaitingTime waitingTime) (Quickness quickness) =
        match waitingTime with
        | w when w = 0 -> quickness
        | w -> max 0 (w - 1)
        |> WaitingTime
        
    let calcTarget gridSize targetGridPos position (Grid grid)  =
        
        let selfGridPosition =
            toGridPosition gridSize position
            
        let inSameGrid =
            targetGridPos = selfGridPosition
        
        let vacants =
            grid
            |> Seq.filter (fun x -> x.Value = 0 )
            |> Array.ofSeq
        
        if inSameGrid && grid.GetValueOrDefault(selfGridPosition, 0) > 1 && (Array.length vacants) > 0
        then
            let t =
                vacants
                |> Seq.minBy (fun x ->
                    let gp = fromGridPosition gridSize x.Key
                    lengthSquared gp position)
            t.Key
        else
            targetGridPos
                    
    let calcDirection gridSize (_:Radian) (Position current) (targetGridPos: GridPosition) =
        let (Position p) = fromGridPosition gridSize targetGridPos
        let diff = p - current
        atan2 diff.Y diff.X |> Radian

    let calcVelocity (Velocity _) (Radian direction) speed =
        Vector2(speed * cos direction, speed * sin direction)
        |> Velocity

    let calcPosition gridSize (Position current) (Velocity velocity) (targetGridPos: GridPosition) =
        let tp = toGridPosition gridSize (Position current)
        
        let (Position p) = fromGridPosition gridSize targetGridPos
        let l1 = (current - p).LengthSquared()
        let l2 = (current + velocity - p).LengthSquared()
        
        if tp = targetGridPos
        then current
        else
            if l1 > l2
            then current + velocity
            else current
        |> Position

    let calcGrid gridSize (Grid grid) (positions: Position seq) =

        let countMap =
            positions
            |> Seq.groupBy (fun p -> toGridPosition gridSize p)
            |> Seq.map (fun (k, values) -> k, Seq.length values)
            |> Map.ofSeq
        
        grid
        |> Map.map (fun pos c -> countMap.GetValueOrDefault(pos, 0))
        |> Grid

    let Update (world: World) =

        let gridSelector (comps: Component<Grid> seq) (_: Component<_>) =
            comps |> Seq.head
            
        let nextWaitingTimes =
            world.WaitingTimes
            |> nextValueWithSameEntity2 calcWaitingTime world.Quickness 
            |> List.ofSeq

        let nextTargets =
            world.TargetPositions
            |> withEntitySelector3
                   (sameEntitySelector world.CurrentPositions)
                   (gridSelector world.Grids)
            |> Seq.map (fun (entityId, target, position, grid) ->
                                    {
                                        EntityId = entityId
                                        Value = calcTarget world.GridSize target position grid
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
            calcGrid world.GridSize world.Grids.Head.Value (nextPosition |> Seq.map (fun p -> p.Value))

        let nextGrid =
            [ { EntityId = EntityId(0)
                Value = nextGridInfo } ]

        { GridSize = world.GridSize
          Quickness = world.Quickness 
          WaitingTimes = nextWaitingTimes 
          TargetPositions = nextTargets
          Directions = nextDirection
          Speeds = world.Speeds
          Velocities = nextVelocity
          CurrentPositions = nextPosition
          Grids = nextGrid }


