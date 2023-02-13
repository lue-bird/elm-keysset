module Order exposing
    ( Ordering
    , tie, Tie
    , reverse, Reverse, ReverseTag
    , by, By, ByTag
    , onTie, OnTieNext, OnTieNextTag
    , on, On, OnTag
    , with
    )

{-| Comparing things

Build tagged `compare` functions for basic types, records, `type` choices.
All without `comparable`, `number`, ...

    import Order exposing (Ordering)
    import Int.Order
    import Char.Order
    import String.Order
    import Map exposing (Mapping)

    type FirstName
        -- no exposing (..)
        = FirstName

    firstName : Mapping User FirstName String
    firstName =
        Typed.tag FirstName (\(User user) -> user.firstName)

    type LastName
        -- no exposing (..)
        = LastName

    lastName : Mapping User LastName String
    lastName =
        Typed.tag LastName (\(User user) -> user.lastName)

    type Age
        -- no exposing (..)
        = Age

    age : Mapping User Age String
    age =
        Typed.tag Age (\(User user) -> user.age)

    type User
        = User
            { firstName : String
            , lastName : String
            , age : Int
            }

    type alias NameOrder =
        String.Order.Earlier
            (Char.Order.Alphabetically Char.Order.LowerUpper)

    type alias Order =
        Order.NextOnTie
            (Order.By LastName NameOrder)
            (Order.NextOnTie
                (Order.By FirstName NameOrder)
                (Order.By Age Order.Int.Increasing)
            )

    order : Ordering User Order
    order =
        Order.by lastName
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


### ordering core types

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

@docs on, On, OnTag


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
import Typed


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


{-| Tags an attached [`on`](#on) possibility
[`Mapping`](Map#Mapping)-[`Ordering`](#Ordering) pair
-}
type alias On toPossibility order =
    ( OnTag, ( toPossibility, order ) )


{-| Tag wrapper for [`On`](#On)
-}
type OnTag
    = On


{-| Try ordering by a given parsed value.
In comparison to [`Order.by`](#by), mapping can produce `Nothing` in which case any `Just` value is greater.

Continue by adding all possibilities with [`Order.onTie`](#onTie)

    module Card exposing (Card(..), CardNormal, order)

    import Order exposing (Ordering)
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

    type Card
        = Normal CardNormal
        | Joker

    type alias CardNormal =
        RecordWithoutConstructorFunction
            { value : Value, suite : Suite }

    type ToNormal
        = ToNormal

    normal : Mapping Card ToNormal (Maybe CardNormal)
    normal =
        Typed.tag ToNormal
            (\card ->
                case card of
                    Normal normalCard ->
                        Just normalCard

                    Joker ->
                        Nothing
            )

    type alias NormalOrder =
        Order.OnTieNext
            (Order.By Suite SuiteOrder)
            (Order.By Value ValueOrder)

    normalOrder : Ordering CardNormal NormalOrder
    normalOrder =
        Order.by suite suiteOrder
            |> Order.onTie
                (Order.by value valueOrder)

    order : Ordering Card (Order.On ToNormal NormalOrder)
    order =
        Order.on toNormal normalOrder

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

_-- Advanced territory --_

Recursive types need special treatment so we don't run into recursive types:
Recursive [`Ordering`](#Ordering)s need a separate tag, e.g.

    import Map
    import Order
    import Typed

    type alias Earlier elementOrder =
        Order.OnTieNext
            (Order.On Empty Order.Tie)
            (Order.On
                Cons
                (Order.OnTieNext
                    (Order.By ConsElement elementOrder)
                    (Order.By ConsList ( EarlierRecursive, elementOrder ))
                )
            )

    type Empty
        = Empty

    toEmpty : Mapping (List element) Empty (Maybe ())
    toEmpty =
        Typed.tag Empty
            (\list ->
                case list of
                    [] ->
                        Just ()

                    _ :: _ ->
                        Nothing
            )

    type Cons
        = Cons

    toCons : Mapping (List element) Cons (Maybe { element : element, list : List element })
    toCons =
        Typed.tag Cons
            (\list ->
                case list of
                    [] ->
                        Nothing

                    element :: list ->
                        Just { element = element, list = list }
            )

    type ConsElement
        = ConsElement

    consElement : Mapping { element : element, list : List element } ConsList (List element)
    consElement =
        Typed.tag ConsList .list

    type ConsList
        = ConsList

    consList : Mapping { element : element, list : List element } ConsList (List element)
    consList =
        Typed.tag ConsList .list

    type EarlierRecursive
        = EarlierRecursive

    order : Ordering element elementOrder -> Ordering (List element) (Earlier elementOrder)
    order elementOrder =
        let
            listRecursiveOrder : () -> Ordering (List element) ( EarlierRecursive, elementOrder )
            listRecursiveOrder () =
                Typed.mapToWrap EarlierRecursive order
        in
        Order.on toEmpty Order.tie
            |> Order.onTie (Order.on toCons Order.by)

a bit much boilerplate.

-}
on :
    Mapping choice toPossibilityTag (Maybe possibilityAttachment)
    -> Ordering possibilityAttachment possibilityOrderTag
    -> Ordering choice (On toPossibilityTag possibilityOrderTag)
on toPossibility possibilityOrdering =
    toPossibility
        |> Typed.wrapAnd possibilityOrdering
        |> Typed.mapToWrap On
            (\( choose, order ) ( choiceA, choiceB ) ->
                case ( choiceA |> choose, choiceB |> choose ) of
                    ( Nothing, Nothing ) ->
                        EQ

                    ( Nothing, Just _ ) ->
                        LT

                    ( Just _, Nothing ) ->
                        GT

                    ( Just aChosen, Just bChosen ) ->
                        order ( aChosen, bChosen )
            )


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

    cardOrder : Ordering Card (Order.OnTieNext (Order.By Suite SuiteOrder) (Order.By Value ValueOrder))
    cardOrder =
        Order.by suite suiteOrder
            |> Order.onTieNext
                (Order.by value valueOrder)

    type SuiteToInt
        = SuiteToInt

    suiteToInt : Mapping Suite SuiteToInt Int
    suiteToInt =
        Typed.tag SuiteOrder
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

    type SuiteOrder =
        Order.By SuiteToInt Int.Order.Increasing

    suiteOrder : Ordering Suite SuiteOrder
    suiteOrder =
        Order.by suiteToInt Int.Order.increasing

[`onTie`](#onTie) can also be used to order different variants and values → see [`on`](#on)

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
