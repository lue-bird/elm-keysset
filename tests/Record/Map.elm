module Record.Map exposing (AtomicNumber, Char, Closed, Email, Id, Open, Symbol, atomicNumber, char, closed, email, id, open, symbol)

import Map exposing (Mapping)
import Typed


type Id
    = Id


id : Mapping { r_ | id : id } Id id
id =
    Typed.tag Id .id


type Email
    = Email


email : Mapping { r_ | email : email } Email email
email =
    Typed.tag Email .email


type Char
    = Char


char : Mapping { r_ | char : char } Char char
char =
    Typed.tag Char .char


type Open
    = Open


open : Mapping { r_ | open : open } Open open
open =
    Typed.tag Open .open


type Closed
    = Closed


closed : Mapping { r_ | closed : closed } Closed closed
closed =
    Typed.tag Closed .closed


type Symbol
    = Symbol


symbol : Mapping { r_ | symbol : symbol } Symbol symbol
symbol =
    Typed.tag Symbol .symbol


type AtomicNumber
    = AtomicNumber


atomicNumber : Mapping { r_ | atomicNumber : atomicNumber } AtomicNumber atomicNumber
atomicNumber =
    Typed.tag AtomicNumber .atomicNumber
