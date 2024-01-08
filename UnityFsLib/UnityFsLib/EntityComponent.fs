module UnityFsLib.EntityComponent


type EntityId = EntityId of int

// ValueはValueTYpeであることを保証したい
[<Struct>]
type Component<'T> = { EntityId: EntityId; Value: 'T }

// [<Struct>]
// type Component2<'T1, 'T2> =
//     { EntityId: EntityId
//       Value1: 'T1
//       Value2: 'T2 }
//
// [<Struct>]
// type Component3<'T1, 'T2, 'T3> =
//     { EntityId: EntityId
//       Value1: 'T1
//       Value2: 'T2
//       Value3: 'T3 }
//
// [<Struct>]
// type Component4<'T1, 'T2, 'T3, 'T4> =
//     { EntityId: EntityId
//       Value1: 'T1
//       Value2: 'T2
//       Value3: 'T3
//       Value4: 'T4 }

let sameEntitySelector (t2:Component<'T2> seq) (t1:Component<'T1>) =
    t2 |> Seq.find(fun x -> x.EntityId = t1.EntityId)

// let withEntitySelector2old<'F, 'S>
//     (secondSelector: Component<'F> -> Component<'S> seq -> Component<'S>)
//     (second: Component<'S> seq)
//     (first: Component<'F> seq)
//     =
//
//     first
//     |> Seq.map (fun f ->
//         let s = secondSelector f second
//
//         { EntityId = f.EntityId
//           Value1 = f.Value
//           Value2 = s.Value })
    
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

// let withEntitySelector3old<'F, 'S, 'T>
//     (thirdSelector: Component<'F> -> Component<'T> seq -> Component<'T>)
//     (third: Component<'T> seq)
//     (secondSelector: Component<'F> -> Component<'S> seq -> Component<'S>)
//     (second: Component<'S> seq)
//     (first: Component<'F> seq)
//     =
//
//     first
//     |> Seq.map (fun f ->
//         let s = secondSelector f second
//         let t = thirdSelector f third
//
//         { EntityId = f.EntityId
//           Value1 = f.Value
//           Value2 = s.Value
//           Value3 = t.Value })
//
// let withEntitySelector4old<'T1, 'T2, 'T3, 'T4>
//     (fourthSelector: Component<'T1> -> Component<'T4> seq -> Component<'T4>)
//     (fourth: Component<'T4> seq)
//     (thirdSelector: Component<'T1> -> Component<'T3> seq -> Component<'T3>)
//     (third: Component<'T3> seq)
//     (secondSelector: Component<'T1> -> Component<'T2> seq -> Component<'T2>)
//     (second: Component<'T2> seq)
//     (first: Component<'T1> seq)
//     =
//
//     first
//     |> Seq.map (fun t1 ->
//         let t2  = secondSelector t1 second
//         let t3 = thirdSelector t1 third
//         let t4 = fourthSelector t1 fourth
//
//         { EntityId = t1.EntityId
//           Value1 = t1.Value
//           Value2 = t2.Value
//           Value3 = t3.Value
//           Value4 = t4.Value })

let findByEntityId (c: Component<_>) (components: Component<_> seq) =
    components |> Seq.find (fun x -> x.EntityId = c.EntityId)

// let withSameEntity2old<'F, 'S> (second: Component<'S> seq) (first: Component<'F> seq) =
//     first |> withEntitySelector2old findByEntityId second
//
// let withSameEntity3old<'F, 'S, 'T> (third: Component<'T> seq) (second: Component<'S> seq) (first: Component<'F> seq) =
//     first |> withEntitySelector3old findByEntityId third findByEntityId second
//
// let withSameEntity4old<'T1, 'T2, 'T3, 'T4> (fourth: Component<'T4> seq) (third: Component<'T3> seq) (second: Component<'T2> seq) (first: Component<'T1> seq) =
//     first |> withEntitySelector4old findByEntityId fourth findByEntityId third findByEntityId second

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
