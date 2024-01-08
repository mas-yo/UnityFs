module UnityFsLib.EntityComponent


type EntityId = EntityId of int

// ValueはValueTYpeであることを保証したい
[<Struct>]
type Component<'a> = { EntityId: EntityId; Value: 'a }

[<Struct>]
type Component2<'a, 'b> =
    { EntityId: EntityId
      Value1: 'a
      Value2: 'b }

[<Struct>]
type Component3<'a, 'b, 'c> =
    { EntityId: EntityId
      Value1: 'a
      Value2: 'b
      Value3: 'c }

// let firstValue (cmp: Component<_>) =
//     fst cmp.Value
//
// let secondValue (cmp: Component<_>) =
//     fst (snd cmp.Value)
//
// let thirdValue (cmp: Component<_>) =
//     fst (snd (snd cmp.Value))
//
// let valueSetOf1 a =
//     a, ()
//
// let valueSetOf2 a b =
//     a, (b, ())
//
// let valueSetOf3 a b c =
//     a, (b, (c, ()))

let withEntitySelector2<'F, 'S>
    (secondSelector: EntityId -> Component<'S> seq -> Component<'S>)
    (second: Component<'S> seq)
    (first: Component<'F> seq)
    =

    first
    |> Seq.map (fun f ->
        let s = secondSelector f.EntityId second

        { EntityId = f.EntityId
          Value1 = f.Value
          Value2 = s.Value })

let withEntitySelector3<'F, 'S, 'T>
    (thirdSelector: EntityId -> Component<'T> seq -> Component<'T>)
    (secondSelector: EntityId -> Component<'S> seq -> Component<'S>)
    (third: Component<'T> seq)
    (second: Component<'S> seq)
    (first: Component<'F> seq)
    =

    first
    |> Seq.map (fun f ->
        let s = secondSelector f.EntityId second
        let t = thirdSelector f.EntityId third

        { EntityId = f.EntityId
          Value1 = f.Value
          Value2 = s.Value
          Value3 = t.Value })

let findByEntityId entityId (components: Component<_> seq) =
    components |> Seq.find (fun x -> x.EntityId = entityId)

let withSameEntity2<'F, 'S> (second: Component<'S> seq) (first: Component<'F> seq) =
    first |> withEntitySelector2 findByEntityId second
// first |> Seq.map(fun f ->
//     let s = findByEntityId second f.EntityId
//     {
//         EntityId = f.EntityId
//         Value1 = f.Value
//         Value2 = s.Value
//     })

let withSameEntity3<'F, 'S, 'T> (third: Component<'T> seq) (second: Component<'S> seq) (first: Component<'F> seq) =
    first |> withEntitySelector3 findByEntityId findByEntityId third second

// first |> Seq.map(fun f ->
//     let s = findByEntityId second f.EntityId
//     let t = findByEntityId third f.EntityId
//     {
//         EntityId = f.EntityId
//         Value1 = f.Value
//         Value2 = s.Value
//         Value3 = t.Value
//     })



// let withEntity2
//     (components1: Component<_> seq)
//     (components2: Component<_> seq)
//     (components3: Component<_> seq) =
//     components1 |> withEntity components2 |> withEntity components3
//     |> Seq.map (fun x -> secondValue x)
// components1 |> Seq.map(fun s ->
//     {
//         EntityId = s.EntityId
//         ValueSet = (s.Value, (findByEntityId components2 s.EntityId).Value)
//     })
