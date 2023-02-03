module List.Order exposing (earlier, Earlier)

{-| `List` [`Ordering`](Order#Ordering)

@docs earlier, Earlier

-}

import Order exposing (Ordering)
import Typed


{-| Tag for [`earlier`](#earlier)
-}
type Earlier
    = Earlier


{-| Order `List`s by elements first to last

    import Order
    import Int.Order
    import List.Order

    Order.with (List.Order.earlier Int.Order.increasing)
        [ 11, 22, 33, 188 ]
        [ 11, 22, 34 ]
    --> LT

-}
earlier :
    Ordering element elementTag
    -> Ordering (List element) ( Earlier, elementTag )
earlier elementOrdering =
    Typed.mapToWrap Earlier
        (\elementOrder lists ->
            case lists of
                ( [], [] ) ->
                    EQ

                ( [], _ :: _ ) ->
                    LT

                ( _ :: _, [] ) ->
                    GT

                ( head0 :: tail0, head1 :: tail1 ) ->
                    onEQ (\() -> Order.with (earlier elementOrdering) tail0 tail1)
                        (elementOrder ( head0, head1 ))
        )
        elementOrdering


onEQ : (() -> Order) -> (Order -> Order)
onEQ orderBreakingTie =
    \order_ ->
        case order_ of
            LT ->
                LT

            EQ ->
                orderBreakingTie ()

            GT ->
                GT
