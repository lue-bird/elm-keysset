module Order exposing
    ( Ordering
    , tie, Tie
    , reverse, Reverse, ReverseTag
    , by, By, ByTag
    , onTie, OnTieNext, OnTieNextTag
    , choice, ChoiceOrderingBeingBuilt, Choice
    , on, On, OnTag
    , choiceFinish
    , with
    )

{-| Comparing 2 things

Build `compare` operations for basic types, records, `type` choices.
All without `comparable`, `number`, ...

    import Order exposing (Ordering)
    import Int.Order
    import Char.Order
    import String.Order

    type User
        = User
            { firstName : String
            , lastName : String
            , age : Int
            }

    userOrder : Ordering User TODO
    userOrder =
        Order.by (\(User user) -> user)
            (Order.by .lastName
                (String.Order.earlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
                )
                |> Order.onTie
                    (Order.by .firstName
                        (String.Order.earlier
                            (Char.Order.alphabetically Char.Order.lowerUpper)
                        )
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

  - [`Char.Order`](Char-Order)
  - [`Int.Order`](Int-Order)
  - [`Float.Order`](Float-Order)
  - in [`String.Order`](String-Order)
  - in [`List.Order`](List-Order)


### alter

@docs reverse, Reverse, ReverseTag


## combine

@docs by, By, ByTag
@docs onTie, OnTieNext, OnTieNextTag


## choose

@docs choice, ChoiceOrderingBeingBuilt, Choice
@docs on, On, OnTag
@docs choiceFinish


## transform

@docs with


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


{-| Tag for [`tie`](#tie)
-}
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


{-| Tag for [`by`](#by)
-}
type alias By mapTag mappedOrderTag =
    ( ByTag, ( mapTag, mappedOrderTag ) )


{-| Wrapper tag for [`By`](#By)
-}
type ByTag
    = By


{-| Order by a transformed value


#### access a part

    import Typed
    import String.Order
    import Char.Order
    import Map exposing (Mapping)

    type Name
        = Name

    name : Mapping { name : String } Name String
    name =
        Typed.tag Name .name

    [ { name = "Bo" }, { name = "Amy" }, { name = "Cam" } ]
        |> (List.sortWith << Order.with)
            (Order.by name
                (String.Order.earlier (Char.Order.alphabetically Order.tie))
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

    import Char.Order
    import Order exposing (Ordering)
    import String.Order

    type User
        = User { name : String }

    userNameOrder : Ordering Username
    userNameOrder =
        Order.by (\(User user) -> user)
            (Order.by .name
                (String.Order.earlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
                )
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
        Order.choice
            (\card ->
                case card of
                    -- match all variants with _values_
                    Normal _ ->
                        ()

                    Joker ->
                        ()
            )
            |> Order.on toNormal normalOrder

    type Normal
        = Normal

    toNormal : Mapping Card Normal (Maybe CardNormal)
    toNormal =
        \card ->
            case card of
                Normal normal ->
                    normal |> Just

                Joker ->
                    Nothing

neat!

-}
by :
    Mapping complete mapTag mapped
    -> Ordering mapped mappedOrderTag
    -> Ordering complete (By mapTag mappedOrderTag)
by map mappedOrder =
    map
        |> Typed.wrapAnd mappedOrder
        |> Typed.mapToWrap By
            (\( change, mappedOrderWith ) ->
                \toOrder ->
                    toOrder
                        |> Tuple.mapBoth change change
                        |> mappedOrderWith
            )


{-| Choice [`Ordering`](#Ordering) construction in progress.

1.  start building a choice [`Ordering`](#Ordering) with [`Order.choice`](#choice),
2.  specify possibilities with [`on`](#on)
3.  [`choiceFinish`](#choiceFinish)

-}
type alias ChoiceOrderingBeingBuilt choice tags =
    Typed
        Checked
        ( Choice, tags )
        Public
        (List
            { is : choice -> Maybe ()
            , order : ( choice, choice ) -> Maybe Order
            }
        )


{-| Tags a choice [`Ordering`](#Ordering) and [`ChoiceOrderingBeingBuilt`](#ChoiceOrderingBeingBuilt).

Start building a choice [`Ordering`](#Ordering) with [`Order.choice`](#choice)

-}
type Choice
    = Choice


{-| Introduce a choice [`Ordering`](#Ordering) [builder](#ChoiceOrderingBeingBuilt).

The argument should look like

    \aOrBOrC ->
        case aOrBOrC of
            A ->
                ()

            B ->
                ()

            C ->
                ()

as a reminder for you to [add added possibilities to the builder](#on).

Continue the builder by adding all possibilities with [`Order.on`](#on)

TODO

-}
choice :
    (choice -> ())
    -> ChoiceOrderingBeingBuilt choice ()
choice _ =
    Typed.tag ( Choice, () ) []


{-| Tags an attached [`on`](#on) possibility
[`Mapping`](Map#Mapping)-[`Ordering`](#Ordering) pair
-}
type alias On toPossibilityTag possibilityOrderTag =
    ( OnTag, ( toPossibilityTag, possibilityOrderTag ) )


{-| Tag wrapper for [`On`](#On)
-}
type OnTag
    = On


on :
    Mapping choice toPossibilityTag (Maybe possibilityAttachment)
    -> Ordering possibilityAttachment possibilityOrderTag
    ->
        (ChoiceOrderingBeingBuilt choice tags
         ->
            ChoiceOrderingBeingBuilt
                choice
                ( tags, On toPossibilityTag possibilityOrderTag )
        )
on toPossibility possibilityAttachmentOrdering =
    \choiceOrderingInProgress ->
        choiceOrderingInProgress
            |> Typed.mapUnwrap identity
            |> Typed.wrapAnd
                (toPossibility
                    |> Typed.wrapAnd possibilityAttachmentOrdering
                    |> Typed.mapToWrap On
                        (\( to, order ) ->
                            { to = to, order = order }
                        )
                )
            |> Typed.mapToWrap Choice
                (\( possibilitiesSoFar, possibility ) ->
                    possibilitiesSoFar
                        |> (::)
                            { is =
                                \choice_ ->
                                    choice_ |> possibility.to |> Maybe.map (\_ -> ())
                            , order =
                                \( choiceA, choiceB ) ->
                                    Maybe.map2 (\a b -> ( a, b ) |> possibility.order)
                                        (choiceA |> possibility.to)
                                        (choiceB |> possibility.to)
                            }
                )


{-| TODO

    type OneOrOther
        = One String
        | Other String

    Order.choice
        (\choice ->
            case choice of
                One oneAttachment ->
                    ()

                Other otherAttachment ->
                    ()
        )
        |> Order.on toOne stringEarlier
        |> Order.on ( OtherTag, .other ) stringEarlier
        |> Order.choiceFinish

    type ToOne
        = ToOne

    toOne : Mapping OneOrOther ToOne String
    toOne =
        Typed.tag ToOne
            (\choice ->
                case choice of
                    One oneAttachment ->
                        oneAttachment |> Just

                    _ ->
                        Nothing
            )

    type ToOther
        = ToOther

    toOther : Mapping OneOrOther ToOther String
    toOther =
        Typed.tag ToOther
            (\choice ->
                case choice of
                    Other otherAttachment ->
                        otherAttachment |> Just

                    _ ->
                        Nothing
            )

-}
choiceFinish :
    ChoiceOrderingBeingBuilt choice tags
    -> Ordering choice ( Choice, tags )
choiceFinish =
    \choiceOrderingComplete ->
        choiceOrderingComplete
            |> Typed.map
                (\possibilities choices ->
                    let
                        orderedByEqualPossibility =
                            possibilities
                                |> List.foldl
                                    (\possibility soFar ->
                                        case soFar of
                                            Just alreadyOrder ->
                                                alreadyOrder |> Just

                                            Nothing ->
                                                choices |> possibility.order
                                    )
                                    Nothing
                    in
                    case orderedByEqualPossibility of
                        Just order ->
                            order

                        Nothing ->
                            let
                                possibilityIndexes leftOrRight =
                                    possibilities
                                        |> listFirstJustIndex
                                            (\possibility ->
                                                possibility.is (choices |> leftOrRight)
                                            )
                            in
                            case ( possibilityIndexes Tuple.first, possibilityIndexes Tuple.second ) of
                                ( Just a, Just b ) ->
                                    compare a b

                                -- can only be `Nothing` when there aren't
                                -- as many `on`s as possibilities (which is always not intended)
                                ( Just _, Nothing ) ->
                                    GT

                                ( Nothing, Just _ ) ->
                                    LT

                                ( Nothing, Nothing ) ->
                                    EQ
                )
            |> Typed.wrapToChecked Choice


listFirstJustIndex :
    (element -> Maybe ())
    -> (List element -> Maybe Int)
listFirstJustIndex isFound =
    \list ->
        list
            |> List.foldl
                (\el soFar ->
                    case soFar of
                        Ok alreadyFound ->
                            alreadyFound |> Ok

                        Err indexSoFar ->
                            case el |> isFound of
                                Just () ->
                                    indexSoFar |> Ok

                                Nothing ->
                                    indexSoFar + 1 |> Err
                )
                (0 |> Err)
            |> Result.toMaybe


{-| Tag for [`onTie`](#onTie)
-}
type alias OnTieNext orderTag tieBreakingOrderTag =
    ( OnTieNextTag, ( orderTag, tieBreakingOrderTag ) )


{-| Tag wrapper for [`OnTieNext`](#OnTieNext)
-}
type OnTieNextTag
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
            |> Typed.mapToWrap OnTieNext
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


{-| Tag for [`reverse`](#reverse)
-}
type alias Reverse orderTag =
    ( ReverseTag, orderTag )


{-| Tag wrapper for [`Reverse`](#Reverse)
-}
type ReverseTag
    = Reverse


{-| `a < b  ⇆  a > b`

    import Char.Order
    import String.Order

    [ "b", "c", "a" ]
        |> (List.sortWith << Order.with)
            (String.Order.earlier
                (Char.Order.alphabetically Order.tie)
            )
    --> [ "a", "b", "c" ]

    [ "b", "c", "a" ]
        |> (List.sortWith << Order.with)
            (String.Order.earlier
                (Char.Order.alphabetically Order.tie)
                |> Order.reverse
            )
    --> [ "c", "b", "a" ]

-}
reverse : Ordering complete tag -> Ordering complete (Reverse tag)
reverse =
    \ordering ->
        ordering
            |> Typed.mapToWrap Reverse
                (\order ( o0, o1 ) ->
                    -- ↓ parts flipped
                    order ( o1, o0 )
                )
