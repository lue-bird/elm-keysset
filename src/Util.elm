module Util exposing (recover, when)

{-| -}


when : Bool -> (thing -> thing) -> (thing -> thing)
when condition conditionalChange =
    if condition then
        conditionalChange

    else
        identity


recover : (error -> value) -> Result error value -> value
recover errorToValue =
    \result ->
        case result of
            Ok value ->
                value

            Err error ->
                error |> errorToValue
