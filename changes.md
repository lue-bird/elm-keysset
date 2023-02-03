### 3.1.0 plans

  - add an `elm-review` tool to auto-generate `Record.Map`

# change log

## 3.0.0

- `KeySet`, `KeysSet` merge
    - 👍 `KeysSet` functionality while still running in `log n`
    - 👎 minimally more complex API
    - `elementRemove` name → `remove`
    - `only` name → `one`
    - `mapTry` type result keep `possiblyOrNever`
- `.Order` modules move from `linear-direction`
    - `module Maybe.Order` remove
    - `module Case` move into `Char.Linear`
    - integrate tag into `Order`, so that practically no manual opaque rules are needed,
    for example
    ```elm
    Int.Order.increasing
    --: Ordering Int Int.Order.Increasing

    Order.by Record.Map.name Int.Order.increasing
        |> Order.onTie
            (Order.by Record.Map.status
                (String.Order.earlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
                )
            )
    --: Ordering
    --:     User
    --:     (Order.OnTieNext
    --:         (Order.By Record.Map.Name Int.Order.Increasing)
    --:         (Order.By Record.Map.Status
    --:             (String.Order.earlier
    --:                 (Char.Order.Alphabetically Char.Order.LowerUpper)
    --:             )
    --:         )
    --:     )
    ```
    with per project one
    ```elm
    module Record.Map exposing (Name, name, Status, status)

    import Typed exposing (Typed, Internal, Public, tag, isChecked)

    type Name -- no (..)
        = Name
    
    type Status -- no (..)
        = Status
    
    name : Typed Internal Name Public ({ record | name : name } -> name)
    name =
        .name |> tag Name |> isChecked Name
    
    status : Typed Internal Status Public ({ record | status : status } -> status)
    status =
        .status |> tag Status |> isChecked Status
    ```
        - 👎 chaining with `onTie` is slightly more verbose than `onTieNext [ ... ]`
        - 👍 chaining with `onTie` is more obvious and easier to read than `onTieNext [ ... ]`

#### 2.1.1

- readme `KeySet` example ordering correct

### 2.1.0

- `KeySet` add

## 2.0.0

- indirection `ElementsWithUniquenessPromises` remove
- `when` remove
    - in favor of `mapTry`
- `set |> isUnique element` → `element |> allDifferentFrom set`
- `equal a b` → `a |> isEqualTo b`
- `at door key` → `element ( door, key )`
- `update door key` → `elementAlter ( door, key )`
- `updateAll` name → `alter`
- `remove` name → `elementRemove`
- `insertAll` name → `insertList`
- `mapTry` add
- `lue-bird/elm-typed-value` dependency remove
    - in favor of opaque `type` immune to internal changes
- performance improve

## 1.0.0

- removed `KeysDict.toDict`
- removed `KeysDict.serialize`
- removed `KeysDict.Uniqueness.violated`
- changed `KeysDict.enterBy { door, key }` to `at door key`
- changed `KeysDict.remove { door, key }` to `remove door key`

- renamed `KeysDict` to `KeysSet`
- moved `KeysDict.Uniqueness.Uniqueness` to `KeysSet.Uniqueness`
- moved `KeysDict.Uniqueness.unique` to `KeysSet.unique`
- renamed `KeysDict.foldHouses` to `fold`
- renamed `KeysDict.countHouses` to `size`
- renamed `KeysDict.houses` to `toList`
- renamed `KeysDict.enterableBy` to `promising`

- added `KeysSet.insertAll`
- added `KeysSet.update`
- added `KeysSet.updateAll`
- added `KeysSet.where`
- added `KeysSet.isUnique`
- added `KeysSet.all`
- added `KeysSet.any`
- added `KeysSet.isEmpty`

## before
[indique/elm-keysdict](https://package.elm-lang.org/packages/indique/elm-keysdict/latest/) → `lue-bird/elm-keysset`
