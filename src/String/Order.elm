module String.Order exposing (earlier, Earlier, EarlierTag)

{-| `String` [`Ordering`](Order#Ordering)

@docs earlier, Earlier, EarlierTag

-}

import Order exposing (Ordering)
import Typed


{-| Tag for [`earlier`](#earlier)
-}
type alias Earlier elementOrder =
    ( EarlierTag, elementOrder )


{-| Wrapper tag for [`Earlier`](#Earlier)
-}
type EarlierTag
    = Earlier


{-| `Order` `String`s by `Char`s first to last, specifying a [`Char` `Ordering`](Char-Order):

    import Order
    import Char.Order
    import String.Order

    Order.with
        (String.Order.earlier
            (Char.Order.aToZ Char.Order.lowerUpper)
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
earlier :
    Ordering Char charTag
    -> Ordering String (Earlier charTag)
earlier charOrdering =
    Typed.mapToWrap Earlier
        (\charOrder ( string0, string1 ) ->
            nothingJust
                (\( char0, other0 ) ( char1, other1 ) ->
                    charOrder ( char0, char1 )
                        |> onEQ (\() -> Order.with (earlier charOrdering) other0 other1)
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

    nothingJust Int.Order.up
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
