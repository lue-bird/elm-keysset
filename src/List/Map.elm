module List.Map exposing (each)

{-| [`Map`](Map) a `List`

@docs each

-}

import Map exposing (Mapping)
import Typed


type Each
    = Each


{-| Map each element contained inside a `List`

    import Map
    import Record

    effect : { trail : List { sparkle : Int } }
    effect =
        { trail =
            [ { sparkle = 2 }
            , { sparkle = 3 }
            , { sparkle = 4 }
            ]
        }

    effect
        |> Map.over
            (Record.Map.trail |> Map.over List.Map.each Record.sparkle)
            (\n -> n + 1)
    --> { trail = [ { sparkle = 3 }, { sparkle = 4}, { sparkle = 5 } ] }

-}
each :
    Mapping element elementMapTag elementMapped
    -> Mapping (List element) ( Each, elementMapTag ) (List elementMapped)
each elementMapping =
    Typed.mapToWrap Each List.map elementMapping



{- @future


   type ElementAt
       = ElementAt


   {-| Map a `List`'s element at a given index

       import Map
       import List.Map
       import Record

       sparkles : List { sparkle : String }
       sparkles =
           [ { sparkle = "Stuff" }, { sparkle =  "Things" }, { sparkle = "Woot" } ]

       sparkles
           |> Map.over
               (List.Map.element 0 << Record.sparkle)
               (\_ -> "Whatever")
       --> [ { sparkle = "Whatever" }, { sparkle =  "Things" }, { sparkle = "Woot" } ]

       sparkles
           |> Map.over
               (List.Map.element 9000 << Record.sparkle)
               (\_ -> "Whatever")
       --> sparkles

   -}
   element : N (Exactly index) -> Alter (List element) ( ElementAt, index ) element elementMapTag_
   element index =
       Typed.tag
           (index |> N.min |> N.fixedToValue |> N.fixedToNumber)
           (index |> N.toInt)
           |> Typed.mapToWrap ElementAt
               (\indexInt ->
                   \elementAlter list ->
                       list |> List.updateAt indexInt elementAlter
               )
           |> Map.typed
-}
