﻿namespace UnityFsLib

open System
open System.Collections.Generic
open System.Numerics


module ECS =
    
    [<Struct>]
    type Component<'a> = {
        EntityId : int
        Value : 'a
    }
    
    [<Struct>]
    type Grid = {
        EntityNums: Map<int*int, int>
        GridSize: float32
    }

    [<Struct>]
    type World = {
        MouseClicked: Component<Vector2 option> list
        TargetPositions : Component<Vector2> list
        Directions : Component<float> list
        Speeds : Component<float> list
        Velocities : Component<Vector2> list
        Positions : Component<Vector2> list
        Grid: Component<Grid> list
    }
    
    let addTuple a b =
        let a1, a2 = a
        let b1, b2 = b
        a1 + b1, a2 + b2
   
    let findByEntityId (components: Component<_> seq) entityId =
        components |> Seq.find (fun x -> x.EntityId = entityId)
        
    let getGridIndex (target: Vector2) gridSize =
        (int (target.X / gridSize), int (target.Y / gridSize))

    let calcTarget position (grid: Grid) =
        let gxy = getGridIndex position grid.GridSize
        let gxys = [|
            addTuple gxy (-1, 0);
            addTuple gxy ( 1, 0);
            addTuple gxy ( 0,-1);
            addTuple gxy ( 0, 1);
            addTuple gxy (-1,-1);
            addTuple gxy (-1, 1);
            addTuple gxy ( 1,-1);
            addTuple gxy ( 1, 1);
        |]
        
        let (minx, miny), _ = gxys |> Seq.map (fun g -> g, grid.EntityNums.GetValueOrDefault(g, 0))
                                   |> Seq.minBy (fun (_, c) -> c)
    
        Vector2(float32 minx * grid.GridSize, float32 miny * grid.GridSize)
    
    let calcDirection (current:Vector2) (target:Vector2) =
        let diff = target - current
        Math.Atan2(float diff.Y, float diff.X)
        
    let calcVelocity direction speed =
        Vector2(float32 (Math.Cos(direction) * speed), float32 (Math.Sin(direction) * speed))
        
    
        
    let calcGrid (positions: Component<Vector2> seq) gridSize =
        
        let nums = positions |> Seq.groupBy (fun p -> getGridIndex p.Value gridSize)
                              |> Seq.map (fun (k, values) -> k, Seq.length values)
                              |> Map.ofSeq
        {
            EntityNums = nums
            GridSize = gridSize
        }
        
    let enumerateEntities
        (source: Component<'T> seq) =
        source |> Seq.map(fun s -> (s.EntityId, s.Value))

    let enumerateEntities1
        (source: Component<'T> seq)
        (reference: Component<'U> seq) =
        source |> Seq.map(fun s -> (s.EntityId, s.Value, (findByEntityId reference s.EntityId).Value))

    let enumerateEntities2
        (source: Component<'T> seq)
        (reference1: Component<'U> seq)
        (reference2: Component<'V> seq) =
        source |> Seq.map(fun s -> (s.EntityId, s.Value, (findByEntityId reference1 s.EntityId).Value, (findByEntityId reference2 s.EntityId).Value))
        
    let update world: World =
        
        let targetPosition = enumerateEntities world.Positions
                            |> Seq.map (fun (entityId, p) -> {EntityId = entityId; Value = calcTarget p world.Grid.Head.Value})
        
        let nextDirection = enumerateEntities2 world.Directions targetPosition world.TargetPositions
                            |> Seq.map (fun (entityId, _,c,t) -> {EntityId = entityId; Value = calcDirection c t})
        
        let nextVelocity = enumerateEntities2 world.Velocities nextDirection world.Speeds
                            |> Seq.map (fun (entityId,_,d,s) -> {EntityId = entityId; Value = calcVelocity d s})
                            
        let nextPosition = enumerateEntities1 world.Positions nextVelocity
                           |> Seq.map (fun (entityId,p,v) -> {EntityId = entityId; Value = p + v })
                           
        let nextGrid = enumerateEntities world.Grid
                           |> Seq.map (fun (entityId, g) -> {EntityId = entityId; Value = calcGrid nextPosition 10f })
                            
        {
            TargetPositions = targetPosition |> List.ofSeq
            MouseClicked = world.MouseClicked 
            Directions = nextDirection |> List.ofSeq
            Speeds = world.Speeds
            Velocities = nextVelocity |> List.ofSeq
            Positions = nextPosition |> List.ofSeq
            Grid = nextGrid |> List.ofSeq
        }
        
        
module Duck =
    let inline X v = (^a : (member X : _) v)
    let inline Y v = (^a : (member Y : _) v)

    let inline Z v = (^a : (member Z : _) v)
        
module Move =
    let updateRadian deltaTime currentRadian =
        currentRadian + deltaTime
        
    // let getX_ (radian: float32, radius: float32) =
    //     float32 (float radius * Math.Sin(float radian))

    let getX radian radius =
        radius * Math.Sin radian

    let getY radian radius =
        radius * Math.Cos radian

    // let getY (radian: float32, radius: float32) =
    //     float32 (float radius * Math.Cos(float radian))
    
    // let rayCast position direction maxLength =
    //     Physics.Raycast(Vector3( Duck.X position, Duck.Y position, Duck.Z position), Vector3(1f,2f,3f))
    
    type MoveReturnValue =
        struct
            val radian: float32
            val x: float32
            val y: float32
            new(r,x,y) = { radian = r; x = x; y = y; }
        end
        
    // let move (deltaTime: float32, currentRadian: float32) =
    //     let r = updateRadian deltaTime currentRadian
    //     Physics.Raycast
    //     let x = getX (float r) 5.0
    //     let y = getY (float r) 5.0
    //     MoveReturnValue(r, float32 x, float32 y)
