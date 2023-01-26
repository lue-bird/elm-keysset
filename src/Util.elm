module Util exposing (recover)

{-| -}


recover : (error -> value) -> Result error value -> value
recover errorToValue =
    \result ->
        case result of
            Ok value ->
                value

            Err error ->
                error |> errorToValue
