module Card exposing (Card(..), CardNormal, order)

import Int.Order
import Map exposing (Mapping)
import Order exposing (Ordering)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


type Card
    = Normal CardNormal
    | Joker


type alias CardNormal =
    RecordWithoutConstructorFunction
        { value : Value, suite : Suite }


type alias CardOrder =
    Order.On ToNormal CardNormalOrder


order : Ordering Card CardOrder
order =
    Order.on toNormal normalOrder


type ToNormal
    = ToNormal


toNormal : Mapping Card ToNormal CardNormal
toNormal =
    Map.tag ToNormal
        (\card ->
            case card of
                Normal normal ->
                    Just normal

                Joker ->
                    Nothing
        )


type alias CardNormalOrder =
    Order.OnTieNext
        (Order.By SuiteTag SuiteOrder)
        (Order.By ValueTag ValueOrder)


normalOrder : Ordering CardNormal CardNormalOrder
normalOrder =
    Order.by suite suiteOrder
        |> Order.onTie (Order.by value valueOrder)


type SuiteTag
    = SuiteTag


suite : Mapping CardNormal SuiteTag Suite
suite =
    Map.tag SuiteTag .suite


type ValueTag
    = ValueTag


value : Mapping CardNormal ValueTag Value
value =
    Map.tag ValueTag .value


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


type alias SuiteOrder =
    Order.By SuiteToRank Int.Order.Increasing


type SuiteToRank
    = SuiteToRank


suiteOrder : Ordering Suite
suiteOrder =
    Order.by suiteToRank Int.Order.increasing


suiteToRank : Mapping Suite SuiteToRank Int
suiteToRank =
    Map.tag SuiteToRank
        (\suite_ ->
            case suite_ of
                Clubs ->
                    0

                Hearts ->
                    1

                Diamonds ->
                    2

                Spades ->
                    3
        )


type alias ValueOrder =
    Order.By ValueToRank Int.Order.Increasing


valueOrder : Ordering Value
valueOrder =
    Order.by
        Int.Order.increasing


type ValueToRank
    = ValueToRank


valueToRank : Mapping Value ValueToRank Int
valueToRank =
    Map.tag ValueToRank
        (\value_ ->
            case value_ of
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
