# change log

### 2.1.0

- `KeySet` add

## 2.0.0

- indirection `ElementsWithUniquenessPromises` remove
- `when` remove
    - in favor of `mapTry`
- `set |> isUnique element` → `element |> isUniqueIn set`
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
