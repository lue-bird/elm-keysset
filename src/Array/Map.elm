module Array.Map exposing (each, Each)

{-| [`Map`](Map#Map) an `Array`

@docs each, Each

-}

import Array exposing (Array)
import Map exposing (Mapping)
import Typed


type Each
    = Each


{-| Map each element contained inside an `Array`

    import Array exposing (Array)
    import Map
    import Array.Map
    import Record

    effect : { trail : Array { sparkle : Int } }
    effect =
        { trail =
            Array.fromList [ { sparkle = 2 }, { sparkle = 3 }, { sparkle = 4 } ]
        }

    effect
        |> Map.over
            (Record.trail << Array.Map.each << Record.sparkle)
            (\n -> n + 1)
    --> { trail =
    -->     Array.fromList [ { sparkle = 3 }, { sparkle = 4 }, { sparkle = 5 } ]
    --> }

-}
each :
    Mapping element elementMapTag elementMapped
    -> Mapping (Array element) ( Each, elementMapTag ) (Array elementMapped)
each elementMapping =
    Typed.mapToWrap Each Array.map elementMapping



{- @future
   type Element index
       = Element ( index)


   {-| Focus an `Array` element at a given index in a [direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

       import Array exposing (Array)
       import Map
       import Array.Map
       import Record

       tags : Array { tag : String }
       tags =
           Array.fromList [ { tag = "Stuff" }, { tag =  "Things" }, { tag = "Woot" } ]

       tags
           |> Map.over (Array.Map.element 0 << Record.tag) (\_ -> "Whatever")
       --> Array.fromList
       -->     [ { tag = "Whatever" }, { tag =  "Things" }, { tag = "Woot" } ]

       tags
           |> Map.over
               (Array.Map.element 9000 << Record.tag)
               (\_ -> "Whatever")
       --> tags

   -}
   element :
       N (N.Exactly index)
       -> Altering (Array elementMapped) (Element index) elementMapped elementMapTag
   element index =
       Map.tag (Element (index |> N.min |> N.fixedToValue))
           (\alter array ->
               case array |> Array.get (index |> N.toInt) of
                   Nothing ->
                       array

                   Just elementAtIndex ->
                       array |> Array.set (index |> N.toInt) (elementAtIndex |> alter)
           )
-}
