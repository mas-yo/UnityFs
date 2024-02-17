module UnityFsLib.EntityComponent


type EntityId = EntityId of int

// ValueはValueTYpeであることを保証したい
[<Struct>]
type Component<'T> = { EntityId: EntityId; Value: 'T }

let sameEntitySelector (t2:Component<'T2> seq) (t1:Component<'T1>) =
    t2 |> Seq.find(fun x -> x.EntityId = t1.EntityId)
    
let withEntitySelector2<'T1, 'T2>
    (selector2: Component<'T1> -> Component<'T2>)
    (components1: Component<'T1> seq)
    =
    components1
    |> Seq.map (fun c1 ->
        let c2 = selector2 c1
        c1.EntityId, c1.Value, c2.Value)
    
let withEntitySelector3<'T1, 'T2, 'T3>
    (selector2: Component<'T1> -> Component<'T2>)
    (selector3: Component<'T1> -> Component<'T3>)
    (components1: Component<'T1> seq)
    =
    components1
    |> Seq.map (fun c1 ->
        let c2 = selector2 c1
        let c3 = selector3 c1
        c1.EntityId, c1.Value, c2.Value, c3.Value)

let withEntitySelector4<'T1, 'T2, 'T3, 'T4>
    (selector2: Component<'T1> -> Component<'T2>)
    (selector3: Component<'T1> -> Component<'T3>)
    (selector4: Component<'T1> -> Component<'T4>)
    (components1: Component<'T1> seq)
    =
    components1
    |> Seq.map (fun c1 ->
        let c2 = selector2 c1
        let c3 = selector3 c1
        let c4 = selector4 c1
        c1.EntityId, c1.Value, c2.Value, c3.Value, c4.Value)
    
let withSameEntity2<'T1, 'T2>

    (components2: Component<'T2> seq)
    (components1: Component<'T1> seq)
    =
    components1
    |> withEntitySelector2 (sameEntitySelector components2)

let withSameEntity3<'T1, 'T2, 'T3>
    (components2: Component<'T2> seq)
    (components3: Component<'T3> seq)
    (components1: Component<'T1> seq)
    =
    components1
    |> withEntitySelector3 (sameEntitySelector components2) (sameEntitySelector components3)

let withSameEntity4<'T1, 'T2, 'T3, 'T4>
    (components2: Component<'T2> seq)
    (components3: Component<'T3> seq)
    (components4: Component<'T4> seq)
    (components1: Component<'T1> seq)
    =
    components1
    |> withEntitySelector4 (sameEntitySelector components2) (sameEntitySelector components3) (sameEntitySelector components4)

let findByEntityId (c: Component<_>) (components: Component<_> seq) =
    components |> Seq.find (fun x -> x.EntityId = c.EntityId)

let nextValueWithSameEntity2<'T1, 'T2>
    (calcNext: 'T1 -> 'T2 -> 'T1)
    (components2: Component<'T2> seq)
    (components1: Component<'T1> seq)
    =
    components1 |> withSameEntity2 components2
        |> Seq.map (fun (entityId, value1, value2) -> {EntityId = entityId; Value = calcNext value1 value2})

let nextValueWithSameEntity3<'T1, 'T2, 'T3>
    (calcNext: 'T1 -> 'T2 -> 'T3 -> 'T1)
    (components2: Component<'T2> seq)
    (components3: Component<'T3> seq)
    (components1: Component<'T1> seq)
    =
    components1 |> withSameEntity3 components2 components3
        |> Seq.map (fun (entityId, value1, value2, value3) -> {EntityId = entityId; Value = calcNext value1 value2 value3})

let nextValueWithSameEntity4<'T1, 'T2, 'T3, 'T4>
    (calcNext: 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T1)
    (components2: Component<'T2> seq)
    (components3: Component<'T3> seq)
    (components4: Component<'T4> seq)
    (components1: Component<'T1> seq)
    =
    components1 |> withSameEntity4 components2 components3 components4
        |> Seq.map (fun (entityId, value1, value2, value3, value4) -> {EntityId = entityId; Value = calcNext value1 value2 value3 value4})
