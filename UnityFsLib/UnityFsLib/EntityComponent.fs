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

let withSameEntity3<'F, 'S, 'T> (third: Component<'T> seq) (second: Component<'S> seq) (first: Component<'F> seq) =
    first |> withEntitySelector3 findByEntityId findByEntityId third second
