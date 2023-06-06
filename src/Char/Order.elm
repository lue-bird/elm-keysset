module Char.Order exposing
    ( Case(..)
    , lowerUpper, LowerUpper, upperLower
    , unicode, Unicode
    , aToZ, AToZ, AToZTag
    )

{-| `Order` `Char`s


## casing

@docs Case


### order

@docs lowerUpper, LowerUpper, upperLower


## [`Order`](Order#Ordering)

@docs unicode, Unicode
@docs aToZ, AToZ, AToZTag

-}

import Order exposing (Ordering)
import Typed


{-| Tag for [`lowerUpper`](#lowerUpper)
-}
type LowerUpper
    = LowerUpper


{-| `'a' < 'A'`
-}
lowerUpper : Ordering Case LowerUpper
lowerUpper =
    Typed.tag LowerUpper
        (\cases ->
            case cases of
                ( CaseLower, CaseLower ) ->
                    EQ

                ( CaseLower, CaseUpper ) ->
                    LT

                ( CaseUpper, CaseLower ) ->
                    GT

                ( CaseUpper, CaseUpper ) ->
                    EQ
        )


{-| `'A' < 'a'`
-}
upperLower : Ordering Case (Order.Reverse LowerUpper)
upperLower =
    lowerUpper |> Order.reverse


{-| `Case` of a letter. [`Ordering`](Order#Ordering)s:

  - [`lowerUpper`](#lowerUpper)
  - [`upperLower`](#upperLower)
  - [`Order.tie`](Order#tie)

-}
type Case
    = CaseLower
    | CaseUpper


{-| Tag for [`aToZ`](#aToZ)
-}
type alias AToZ caseOrder =
    ( AToZTag, caseOrder )


{-| Wrapper tag for [`AToZ`](#AToZ)
-}
type AToZTag
    = AToZ


{-| `Order` `Char`s

  - Both are letters → `Order` ascii-alphabetically (a < ... < z)
      - They're the same letter? → a given [`Ordering`](Order#Ordering) on their [cases](#Case)
  - Both aren't letters → `Order` [according to unicode char code](#unicode)
  - Only one is a letter → the letter is considered greater

```
Order.with (Char.Order.aToZ Char.Order.upperLower) 'b' 'D'
--> LT

Order.with (Char.Order.aToZ Char.Order.upperLower) 'l' 'L'
--> GT

Order.with (Char.Order.aToZ Char.Order.upperLower) 'i' '!'
--> GT

Order.with (Char.Order.aToZ Char.Order.upperLower) '-' '!'
--> Order.with Char.Order.unicode '-' '!'
```

-}
aToZ : Ordering Case charOrderTag -> Ordering Char (AToZ charOrderTag)
aToZ caseOrdering =
    Typed.mapToWrap AToZ
        (\caseOrder ( char0, char1 ) ->
            case ( char0 |> charCase, char1 |> charCase ) of
                ( Just case0, Just case1 ) ->
                    Order.with unicode (char0 |> Char.toLower) (char1 |> Char.toLower)
                        |> onEQ (\() -> caseOrder ( case0, case1 ))

                ( Nothing, Just _ ) ->
                    LT

                ( Just _, Nothing ) ->
                    GT

                ( Nothing, Nothing ) ->
                    Order.with unicode char0 char1
        )
        caseOrdering


charCase : Char -> Maybe Case
charCase =
    \char_ ->
        if char_ |> Char.isLower then
            CaseLower |> Just

        else if char_ |> Char.isUpper then
            CaseUpper |> Just

        else
            Nothing


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


{-| [`Ordering`](Order#Ordering) according to the unicode char code.

This behavior matches elm's `Basics.compare`.

-}
unicode : Ordering Char Unicode
unicode =
    Typed.tag Unicode (\( a, b ) -> compare a b)


{-| Tag for [`unicode`](#unicode)
-}
type Unicode
    = Unicode
