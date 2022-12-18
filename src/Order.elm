module Order exposing
    ( Ordering
    , tie, Tie
    , reverse, Reverse
    , by, By, onTie, OnTieNext
    , with
    , Key, key, byKey, withKey
    )

{-| Comparing 2 things

Build `compare` operations for basic types, records, `type` choices.
All without `comparable`, `number`, ...

    import Order exposing (Ordering)
    import Case
    import Int.Order
    import Char.Order
    import String.Linear

    type User
        = User
            { firstName : String
            , lastName : String
            , age : Int
            }

    userOrder : Ordering User
    userOrder =
        Order.by (\(User user) -> user)
            (Order.by .lastName
                (String.Linear.greaterEarlier (Char.Order.alphabetically Case.lowerUpper))
                |> Order.onTie
                    (Order.by .firstName
                        (String.Linear.greaterEarlier (Char.Order.alphabetically Case.lowerUpper))
                    )
                |> Order.onTie
                    (Order.by .age Int.Order.increasing)
            )

    [ User { firstName = "Andy", lastName = "Baldwin", age = 90 }
    , User { firstName = "Bert", lastName = "Anderson", age = 23 }
    , User { firstName = "Alec", lastName = "Anderson", age = 8 }
    , User { firstName = "Alec", lastName = "Anderson", age = 100 }
    ]
        |> List.sortWith (Order.with userOrder)
    --> [ { firstName = "Alec", lastName = "Anderson", age = 8 }
    --> , { firstName = "Alec", lastName = "Anderson", age = 100 }
    --> , { firstName = "Bert", lastName = "Anderson", age = 23 }
    --> , { firstName = "Andy", lastName = "Baldwin", age = 90 }
    --> ]

[↑ another example](https://github.com/lue-bird/elm-keysset/blob/master/example/card)

@docs Ordering


## create

@docs tie, Tie


### primitive

  - [`Case`](Case)
  - [`Char.Order`](Char-Order)
  - [`Int.Order`](Int-Order)
  - [`Float.Order`](Float-Order)
  - in [`String.Linear`](String-Linear)
  - in [`List.Linear`](List-Linear)


### alter

@docs reverse, Reverse


## combine

@docs by, By, onTie, OnTieNext


## transform

@docs with


## as key

@docs Key, key, byKey, withKey


## prior art

  - [`matthewsj/elm-ordering`](https://dark.elm.dmy.fr/packages/matthewsj/elm-ordering/latest/Ordering)
      - most complete
      - bad naming `byField`: `byField Tuple.first`, ... are also possible
      - clutter `isOrdered`, both `byField` & `byFieldWith`, ...
      - verbose `... |> breakTiesWith ...` chain instead of `onTieNext [ ... ]`
      - missing `list`, `maybe`, ... orderings
  - [`TSFoster/elm-compare`](https://dark.elm.dmy.fr/packages/TSFoster/elm-compare/latest/Compare)
      - API is a bit scuffed: multiple andThens will be nested instead of flat, ...
      - missing `list`, `maybe`, ... compare operations
  - [`rtfeldman/elm-sorter-experiment`](https://dark.elm.dmy.fr/packages/rtfeldman/elm-sorter-experiment/latest/Sort)
      - wrapped in an opaque `type` → more verbose
      - missing `list`, `maybe`, ... sorters
      - `Sort.Set` & `Sort.Dict` are very nice! You can even use `Sort.Set.empty (Sorter.custom Order.x)` if you wanted
  - [`nikita-volkov/`: `Typeclasses-Classes-Comparison`](https://package.elm-lang.org/packages/nikita-volkov/typeclasses/latest/Typeclasses-Classes-Comparison)
      - almost no API, _only_ `int`,`float`,`comparable` & `map`)
      - [`HashingContainers.HashSet`](https://package.elm-lang.org/packages/nikita-volkov/hashing-containers/2.1.0/HashingContainers-HashSet),
        [`HashingContainers.HashDict`](https://package.elm-lang.org/packages/nikita-volkov/hashing-containers/2.1.0/HashingContainers-HashDict)
        are nice!
  - ... know others? → PR

-}

import Map exposing (Mapping)
import Typed exposing (Checked, Public, Typed)


{-| How 2 thing compare to each other.
An `Ordering` can be used to `sortWith`, make comparisons, ...

    cardsSort : List Card -> List Card
    cardsSort =
        List.sortWith (Order.with cardOrder)

    cardOrder : Ordering Card

-}
type alias Ordering complete tag =
    Mapping ( complete, complete ) tag Order


type Tie
    = Tie


{-| Convert to a function in the form `a -> a -> Order`
that can be used to `sortWith`, make comparisons, ...

    cardsSort : List Card -> List Card
    cardsSort =
        List.sortWith (Order.with cardOrder)

    cardOrder : Ordering Card

-}
with : Ordering complete tag_ -> (complete -> complete -> Order)
with ordering =
    \a b -> ( a, b ) |> (ordering |> Typed.untag)


{-| Always `EQ` for any 2 things

    Order.with tie 5555 -12345 --> EQ

-}
tie : Ordering complete_ Tie
tie =
    Typed.tag Tie (\_ -> EQ)


type By mapTag mappedOrderTag
    = By


{-| Order by a transformed value


#### access a part

    import List.Linear
    import String.Linear
    import Char.Order

    [ { name = "Bo" }, { name = "Amy" }, { name = "Cam" } ]
        |> List.Linear.sortWith
            (Order.by Record.Map.name
                (String.Order.greaterEarlier (Char.Order.alphabetically Order.tie))
            )
    --> [ { name = "Amy" }, { name = "Bo" }, { name = "Cam" } ]

    {-| `Order` `Tuple.first`, then on tie `Tuple.second`

        Order.tuple ( Int.Order.increasing, Int.Order.increasing ) ( 0, 2 ) ( 0, -2 )
        --→ GT
    -}
    tuple :
        ( Ordering part0 part0OrderTag
        , Ordering part1 part1OrderTag
        )
        -> Ordering
                ( part0, part1 )
                (Order.OnTieNext
                    (Order.By Tuple.Map.Part0 part0OrderTag)
                    (Order.By Tuple.Map.Part1 part1OrderTag)
                )
    tuple ( part0Order, part1Order ) =
        Order.by Tuple.Map.part0 part0Order
            |> Order.onTie
                (Order.by Tuple.Map.part1 part1Order)


#### unwrap a type union constructor

    import Case
    import Char.Order
    import Order exposing (Ordering)
    import String.Linear

    type User
        = User { name : String }

    userNameOrder : Ordering Username
    userNameOrder =
        Order.by (\(User user) -> user)
            (Order.by .name
                (String.Linear.greaterEarlier (Char.Order.alphabetically Case.lowerUpper))
            )


#### rank a simple choice

    type Case
        = Lower
        | Upper

    {-| `'A' < 'a'`
    -}
    upperLower : Ordering Case
    upperLower =
        Order.by
            (\case_ ->
                case case_ of
                    Upper ->
                        0

                    Lower ->
                        1
            )
            Int.Order.increasing


#### rank a choice

    module Card exposing (Card(..), CardNormal, order)

    import Order exposing (Ordering)
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

    type Card
        = Normal CardNormal
        | Joker

    type alias CardNormal =
        RecordWithoutConstructorFunction
            { value : Value, suite : Suite }

    normalOrder : Ordering CardNormal TODO
    normalOrder =
        Order.by Record.Map.suite suiteOrder
            |> Order.onTie
                (Order.by Record.Map.value valueOrder)

    order : Ordering Card TODO
    order =
        \cards ->
            case cards of
                -- match all variants with _values_
                ( Normal normal0, Normal normal1 ) ->
                    Order.with normalOrder ( normal0, normal1 )

                other ->
                    -- sort others according to tag "rank"
                    Order.with
                        (Order.by cardToRank
                            Int.Order.increasing
                        )
                        other

    cardToRank : Card -> Int
    cardToRank =
        \card ->
            \card ->
                case card of
                    Normal _ ->
                        0

                    Joker ->
                        1

Use ↑ once your type union grows to have lots of variants
where exhaustive matching has n^2 branches

For simple type unions with only 2 variants:

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

neat!

-}
by :
    Mapping complete mapTag mapped
    -> Ordering mapped mappedOrderTag
    -> Ordering complete (By mapTag mappedOrderTag)
by map mappedOrder =
    map
        |> Typed.wrapAnd mappedOrder
        |> Typed.mapTo By
            (\( change, mappedOrderWith ) ->
                \toOrder ->
                    toOrder
                        |> Tuple.mapBoth change change
                        |> mappedOrderWith
            )


{-| Tag for [`onTie`](#onTie)
-}
type OnTieNext orderTag tieBreakingOrderTag
    = OnTieNext


{-| Prioritize the [`Ordering`](#Ordering) by one aspect
and break ties with the following

    import Order exposing (Ordering)
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

    type alias Card =
        RecordWithoutConstructorFunction
            { suite : Suite, value : Value }

([`RecordWithoutConstructorFunction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/)
tricks elm into not creating a `Card` function)

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

    cardOrder : Ordering Card
    cardOrder =
        Order.onTieNext
            [ Order.by .suite suiteOrder
            , Order.by .value valueOrder
            ]

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

-}
onTie :
    Ordering complete tieBreakingTag
    ->
        (Ordering complete tag
         -> Ordering complete (OnTieNext tag tieBreakingTag)
        )
onTie orderingBreakingTie =
    \ordering ->
        ordering
            |> Typed.wrapAnd orderingBreakingTie
            |> Typed.mapTo OnTieNext
                (\( order, orderBreakingTie ) ->
                    \toOrder ->
                        toOrder
                            |> order
                            |> onEQ (\() -> toOrder |> orderBreakingTie)
                )


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


type Reverse orderTag
    = Reverse


{-| `a < b  ⇆  a > b`

    import Char.Order
    import String.Linear

    [ "b", "c", "a" ]
        |> List.sortWith
            (String.Linear.greaterEarlier
                (Char.Order.alphabetically Order.tie)
            )
    --> [ "a", "b", "c" ]

    [ "b", "c", "a" ]
        |> List.sortWith
            (String.Linear.greaterEarlier
                (Char.Order.alphabetically Order.tie)
                |> Order.reverse
            )
    --> [ "c", "b", "a" ]

-}
reverse : Ordering complete tag -> Ordering complete (Reverse tag)
reverse =
    \ordering ->
        Typed.mapTo Reverse
            (\order ( o0, o1 ) ->
                -- ↓ parts flipped
                order ( o1, o0 )
            )
            ordering



-- as key


type alias Key element key byTag =
    Typed
        Checked
        byTag
        Public
        { toKey : element -> key
        , keyOrder : ( key, key ) -> Order
        }


key :
    Mapping element elementToKeyTag key
    -> Ordering key elementKeyComparingTag
    ->
        Key
            element
            key
            (By elementToKeyTag elementKeyComparingTag)
key elementMapToKey elementComparing =
    Typed.mapTo By
        (\( elementToKey, elementCompare ) ->
            { toKey = elementToKey
            , keyOrder = elementCompare
            }
        )
        (elementMapToKey
            |> Typed.wrapAnd elementComparing
        )


{-| Convert the [`Key`](#Key) to an [`Ordering`](#Ordering) with the same tag
-}
byKey :
    Key element key (By mapTag mappedEqualTag)
    -> Ordering element (By mapTag mappedEqualTag)
byKey keyByEqual =
    keyByEqual
        |> Typed.map
            (\key_ ->
                \( a, b ) ->
                    key_.keyOrder
                        ( a |> key_.toKey, b |> key_.toKey )
            )
        |> Typed.toChecked By


{-| Convert the [`Key`](#Key) to a function in the form `element -> element -> Order`
that can be used to `sortWith`, make comparisons, ...

    cardsSort : List Card -> List Card
    cardsSort =
        List.sortWith (Order.withKey cardOrder)

    cardOrder : Order.Key Card (Order.By CardToIndex Int.Order.Increasing) Int

-}
withKey :
    Key element key_ byTag_
    -> (element -> element -> Order)
withKey =
    \key_ ->
        let
            keyParts =
                key_ |> Typed.untag
        in
        \a b ->
            keyParts.keyOrder
                ( a |> keyParts.toKey, b |> keyParts.toKey )
