module Char.Order exposing
    ( Case(..)
    , lowerUpper, LowerUpper, upperLower
    , alphabetically, Alphabetically
    )

{-| `Order` `Char`s


## casing

@docs Case


### order

@docs lowerUpper, LowerUpper, upperLower


## [`Order`](Order#Ordering)

@docs alphabetically, Alphabetically

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


{-| Tag for [`alphabetically`](#alphabetically)
-}
type Alphabetically
    = Alphabetically


{-| `Order` `Char`s

  - Both are letters → `Order` alphabetically
      - They're the same picture? → a given [`Ordering`](Order#Ordering) on their cases
  - Just one isn't a letter → `Order` according to unicode char code

```
Char.Order.alphabetically Char.Order.upperLower 'b' 'D'
--> LT

Char.Order.alphabetically Char.Order.upperLower 'l' 'L'
--> GT
```

-}
alphabetically : Ordering Case charOrderTag -> Ordering Char ( Alphabetically, charOrderTag )
alphabetically caseOrdering =
    -- TODO ('','🌈') should be LT, is GT
    Typed.mapToWrap Alphabetically
        (\caseOrder ( char0, char1 ) ->
            case Maybe.map2 Tuple.pair (char0 |> charCase) (char1 |> charCase) of
                Just ( case0, case1 ) ->
                    compare (char0 |> Char.toLower) (char1 |> Char.toLower)
                        |> onEQ (\() -> caseOrder ( case0, case1 ))

                Nothing ->
                    compare char0 char1
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
