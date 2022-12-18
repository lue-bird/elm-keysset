module Map exposing
    ( over
    , with
    , Altering, Mapping
    )

{-| Map content inside a nested structure

@docs Map, Alter


## combine

@docs over


## transform

@docs with

-}

import Typed exposing (Checked, Public, Typed)


{-| [`Typed`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typed-value/latest/) change

To create, combine

  - a tag that uniquely describes the part
  - a function that changes the `unmapped` to `mapped`

```
-- module Record.Map exposing (Score, score)


import Typed

type Score
    = Score

score : Mapping { record | score : score } Score score
score =
    Typed.tag Score .score
```

    -- module String.Map exposing (each, Each, ToList, toList)
    type ToList
        = ToList

    toList : Mapping String ToList (List Char)
    toList =
        Typed.tag ToList String.toList

    type Each
        = Each

    each :
        Mapping element elementMapTag elementMapped
        -> Mapping (List element) ( Each, elementMapTag ) (List elementMapped)
    each elementMapping =
        Typed.mapToWrap Each List.map elementMapping

There's nothing more to it

Use [`Alter`] if `unmapped` and `mapped` types are the same

-}
type alias Mapping unmapped mapTag mapped =
    Typed Checked mapTag Public (unmapped -> mapped)


{-| [`Mapping`](#Mapping) that will preserve the element type.

`Altering` can be used to simplify argument and result types

    module List.Map exposing (onHead)

    import Map
    import N exposing (Exactly, Fixed, N, n0)

    type Drop dropped
        = Drop dropped

    drop : N (Exactly dropped) -> Altering (List element) (Drop dropped)
    drop droppedLength =
        Typed.tag ( Drop, n0 |> N.min |> N.fixedToNumber )
            (List.drop droppedLength)

This applies to records, `type`s without any type variables or mapping some of many equal-typed elements

-}
type alias Altering subject alterTag =
    Mapping subject alterTag subject



--


{-| Transform elements
as shown in a given [`Map`](#Map) with a given function

    import Record

    -- don't nest like this in practice
    { book = { sales = 0 } }
        |> Map.over (Record.book |> Map.to Record.sales) (\n -> n + 1)
    --> { book = { sales = 1 } }

-}
over :
    Mapping mapped nextMapTag nextMapped
    ->
        (Mapping unmapped structureTag mapped
         -> Mapping unmapped (Over structureTag nextMapTag) nextMapped
        )
over nextMap =
    \map ->
        map
            |> Typed.wrapAnd nextMap
            |> Typed.mapTo Over
                (\( a, b ) -> a >> b)


{-| Tag for [`over`](#over)
-}
type Over mapTag nextMapTag
    = Over



-- transform


with :
    Mapping unmapped mapTag mapped
    -> (unmapped -> mapped)
with mapped =
    mapped |> Typed.untag
