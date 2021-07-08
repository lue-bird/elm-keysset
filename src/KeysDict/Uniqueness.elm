module KeysDict.Uniqueness exposing
    ( Uniqueness, door
    , violated
    )

{-|

@docs Uniqueness, door


## extend functionality

@docs violated

-}

import Util exposing (aspect)


{-| A promise, that there are never 2 elements with equal keys for the same door.

    listWithoutKeys =
        KeysDict.enterableBy
            --no promised Uniqueness
            []

    doorsInCasedLetter =
        [ door .inAlphabet
        , door .lowercase
        , door .uppercase
        ]

    letters =
        KeyssDict.enterableBy doorsInCasedLetter
            |> KeysDict.insert
                { inAlphabet = 0, lowercase = 'a', uppercase = 'A' }
            |> KeysDict.insert
                -- rejected .inAlphabet key 0 is the same!
                -- → The KeysDict doesn't change
                { inAlphabet = 0, lowercase = 'α', uppercase = 'Α' }

-}
type Uniqueness element
    = NeverTrue (EqualInGivenAspect element)


type alias EqualInGivenAspect element =
    element -> element -> Bool


{-| What's a door? It's a spot to access a element if you have a matching key.

Give `door` one aspect of a element, and it will be ensured that this aspect of an element unique for all elements.

    KeysDict.enterableBy
        [ door .lowercase, door .uppercase ]
        |> KeysDict.insert
            { lowercase= 'a', uppercase= 'A' }
            --put up
        |> KeysDict.insert
            { lowercase= 'a', uppercase= 'B' }
            -- checked items: .lowercase key already exists!
            -- → KeysDict is unchanged

-}
door : (element -> keyNeeded_) -> Uniqueness element
door doorInElement =
    NeverTrue (aspect doorInElement (==))


{-| Do 2 values fail to fulfill `Uniqueness`?

    door .name
        |> violated
            { name= "smile", symbol= '😊' }
            { symbol= '🙂', name= "smile" }
    --> True

-}
violated : element -> element -> Uniqueness element -> Bool
violated aElement bElement (NeverTrue equalInAnAspect) =
    equalInAnAspect aElement bElement
