module Util exposing (aspect, firstWhere)

{-| **Should not be exposed.**

Utility functions.

-}


{-| Give a result only regarding a specific aspect of a value.

    aspect .name (==)
      { name= "smile", symbol= '😊' }
      { symbol= '🙂', name= "smile" }
    --> True

    (aspect abs) (>) 10 -20
    --> False

-}
aspect :
    (value -> aspect)
    -> (aspect -> aspect -> result)
    -> value
    -> value
    -> result
aspect accessAspect resultFromAspects a b =
    resultFromAspects
        (accessAspect a)
        (accessAspect b)


{-| The first (from head) element in a `List` where `isFound` is `True`.

    [ 0, 2, 8, 16, 22 ]
        |> firstWhere (\el -> el > 10)
    --> Just 16

    [ { x = 3, y = 5 }, { y = 9, x = 7 } ]
        |> firstWhere (.x >> (==) 0)
    --> Nothing

-}
firstWhere :
    (element -> Bool)
    -> List element
    -> Maybe element
firstWhere isFound list =
    case list of
        [] ->
            Nothing

        element :: rest ->
            if isFound element then
                Just element

            else
                firstWhere isFound rest
