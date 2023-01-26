module Char.Map exposing (ToCode, toCode)

import Map exposing (Mapping)
import Typed


type ToCode
    = ToCode


toCode : Mapping Char ToCode Int
toCode =
    Typed.tag ToCode Char.toCode
