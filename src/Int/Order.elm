module Int.Order exposing (increasing, Increasing, decreasing)

{-| `Order` `Int`s

@docs increasing, Increasing, decreasing

-}

import Order exposing (Ordering)
import Typed


{-| Tag for [`increasing`](#increasing)
-}
type Increasing
    = Increasing


{-| `Order` `Int`s where lower means greater

    import Order

    Order.with Int.Order.increasing 40 2
    --> GT

-}
increasing : Ordering Int Increasing
increasing =
    Typed.tag Increasing (\( a, b ) -> compare a b)


{-| `Order` `Int`s where higher means greater

    import Order

    Order.with Int.Order.decreasing 2 40
    --> GT

-}
decreasing : Ordering Int (Order.Reverse Increasing)
decreasing =
    increasing |> Order.reverse
