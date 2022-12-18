module Card exposing (Card(..), CardNormal, order)

import Order exposing (Ordering)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


type Card
    = Normal CardNormal
    | Joker


type alias CardNormal =
    RecordWithoutConstructorFunction
        { value : Value, suite : Suite }

{- For simple type unions with only 2 variants:

    order : Ordering Card
    order =
        \card0 card1 ->
            case ( card0, card1 ) of
                -- match all variants with _values_
                ( Normal normal0, Normal normal1 ) ->
                    normalOrder normal0 normal1

                ( Normal _, Joker ) ->
                    GT
                
                ( Joker, Normal _ ) ->
                    LT
                
                ( Joker, Joker ) ->
                    EQ

Use ↓ once your type union grows to have lots of variants
where exhaustive matching has n^2 branches

-}
order : Ordering Card
order =
    \card0 card1 ->
        case ( card0, card1 ) of
            -- match all variants with _values_
            ( Normal normal0, Normal normal1 ) ->
                normalOrder normal0 normal1

            ( cardOther0, cardOther1 ) ->
                -- sort others according to tag "rank"
                Order.by
                    (\card ->
                        case card of
                            Normal _ ->
                                0

                            Joker ->
                                1
                    )
                    Int.Order.increasing
                    cardOther0
                    cardOther1




normalOrder : Ordering CardNormal
normalOrder =
    Order.onTieNext
        [ Order.by .suite suiteOrder
        , Order.by .value valueOrder
        ]


type Suite
    = Clubs
    | Hearts
    | Diamonds
    | Spades


type Value
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


suiteOrder : Ordering Suite
suiteOrder =
    Order.by
        (\suite ->
            case suite of
                Clubs ->
                    0

                Hearts ->
                    1

                Diamonds ->
                    2

                Spades ->
                    3
        )
        Int.Order.increasing


valueOrder : Ordering Value
valueOrder =
    Order.by
        (\value ->
            case value of
                Two ->
                    0

                Three ->
                    1

                Four ->
                    2

                Five ->
                    3

                Six ->
                    4

                Seven ->
                    5

                Eight ->
                    6

                Nine ->
                    7

                Ten ->
                    8

                Jack ->
                    9

                Queen ->
                    10

                King ->
                    11

                Ace ->
                    12
        )
        Int.Order.increasing
