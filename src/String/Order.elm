module String.Order exposing (greaterEarlier, GreaterEarlier)

{-| `Order` `String`s

@docs greaterEarlier, GreaterEarlier

-}

import Order exposing (Ordering)
import Typed


type GreaterEarlier
    = GreaterEarlier


{-| `Order` `String`s by `Char`s first to last, specifying a [`Char` `Ordering`](Char-Order):

    import Char.Order
    import String.Linear

    Order.with
        (String.Linear.greaterEarlier
            (Char.Order.alphabetically CaseChar.Order.lowerUpper)
        )
        "hello, human!"
        "hello, Human"
    --> LT

For string-number chunked text,
you can use [`mcordova47/`: `NaturalOrdering.compare`](https://dark.elm.dmy.fr/packages/mcordova47/elm-natural-ordering/latest/NaturalOrdering#compare)

    NaturalOrdering.compare "abc2.tff" "abc10.tff"
    --→ LT

Personally, I'd just store the `String` as something like

    type TextChunked
        = TextChunked ( String, Maybe (List ( Int, String )) )

and order that
to avoid converting too often

-}
greaterEarlier :
    Ordering Char charTag
    -> Ordering String ( GreaterEarlier, charTag )
greaterEarlier charOrdering =
    Typed.mapToWrap GreaterEarlier
        (\charOrder ( string0, string1 ) ->
            nothingJust
                (\( char0, other0 ) ( char1, other1 ) ->
                    charOrder ( char0, char1 )
                        |> onEQ (\() -> Order.with (greaterEarlier charOrdering) other0 other1)
                )
                ( string0 |> String.uncons
                , string1 |> String.uncons
                )
        )
        charOrdering


onEQ : (() -> Order) -> (Order -> Order)
onEQ orderBreakingTie =
    \order ->
        case order of
            LT ->
                LT

            EQ ->
                orderBreakingTie ()

            GT ->
                GT


{-| `Order` `Nothing` < `Just`

    import Int.Order

    nothingJust Int.Order.increasing
        (Just -99999)
        Nothing
    --→ GT

-}
nothingJust :
    (content -> content -> Order)
    -> (( Maybe content, Maybe content ) -> Order)
nothingJust contentOrder =
    \maybes ->
        case maybes of
            ( Nothing, Nothing ) ->
                EQ

            ( Nothing, Just _ ) ->
                LT

            ( Just _, Nothing ) ->
                GT

            ( Just content0, Just content1 ) ->
                contentOrder content0 content1
