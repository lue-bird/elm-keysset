module Int.Order exposing
    ( up, Up
    , down, Down
    )

{-| `Order` `Int`s

@docs up, Up
@docs down, Down

-}

import Order exposing (Ordering)
import Typed


{-| Tag for [`up`](#up)
-}
type Up
    = Up


{-| `Order` `Int`s where lower means greater

    import Order

    Order.with Int.Order.up 40 2
    --> GT

-}
up : Ordering Int Up
up =
    Typed.tag Up (\( a, b ) -> compare a b)


{-| Tag for [`down`](#down)
-}
type alias Down =
    Order.Reverse Up


{-| `Order` `Int`s where higher means greater

    import Order

    Order.with Int.Order.down 2 40
    --> GT

-}
down : Ordering Int Down
down =
    up |> Order.reverse
