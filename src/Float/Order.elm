module Float.Order exposing (up, Up, down)

{-| `Order` `Float`s

@docs up, Up, down

-}

import Order exposing (Ordering)
import Typed


{-| Tag for [`up`](#up)
-}
type Up
    = Up


{-| `Order` `Float`s where lower means greater

    import Order

    Order.with Float.Order.up 40.34 2.1
    --> GT

-}
up : Ordering Float Up
up =
    Typed.tag Up (\( a, b ) -> compare a b)


{-| `Order` `Float`s where higher means greater

    import Order

    Order.with Float.Order.down 2.1 40.34
    --> GT

-}
down : Ordering Float (Order.Reverse Up)
down =
    up |> Order.reverse
