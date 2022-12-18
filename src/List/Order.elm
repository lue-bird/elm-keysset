module List.Order exposing (greaterEarlier, GreaterEarlier)

{-| `List` [`Ordering`](Order#Ordering)

@docs greaterEarlier, GreaterEarlier

-}

import Order exposing (Ordering)
import Typed


type GreaterEarlier
    = GreaterEarlier


{-| Order `List`s by elements first to last

    import Int.Order

    List.Linear.greaterEarlier Int.Order.increasing
        [ 11, 22, 33, 188 ]
        [ 11, 22, 34 ]
    --> LT

TODO rename to earlier

-}
greaterEarlier :
    Ordering element elementTag
    -> Ordering (List element) ( GreaterEarlier, elementTag )
greaterEarlier elementOrdering =
    Typed.mapToWrap GreaterEarlier
        (\elementOrder lists ->
            case lists of
                ( [], [] ) ->
                    EQ

                ( [], _ :: _ ) ->
                    LT

                ( _ :: _, [] ) ->
                    GT

                ( head0 :: tail0, head1 :: tail1 ) ->
                    onEQ (\() -> Order.with (greaterEarlier elementOrdering) tail0 tail1)
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
