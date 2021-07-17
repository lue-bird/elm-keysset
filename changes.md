# changelog

Continuing [indique/elm-keysdict](https://package.elm-lang.org/packages/indique/elm-keysdict/latest/) as `lue-bird/elm-multiset`.

## 1.0.0

- removed `KeysDict.toDict`
- removed `KeysDict.Uniqueness.violated`
- changed `KeysDict.enterBy { door, key }` to `at door key`
- changed `KeysDict.remove { door, key }` to `remove door key`

- moved `KeysDict.Uniqueness.Uniqueness` to `MultiSet.Uniqueness`
- moved `KeysDict.Uniqueness.unique` to `MultiSet.unique`

- renamed `KeysDict.foldHouses` to `fold`
- renamed `KeysDict.countHouses` to `size`
- renamed `KeysDict.houses` to `toList`
- renamed `KeysDict.enterableBy` to `promising`

- added `MultiSet.insertAll`
- added `MultiSet.update`
- added `MultiSet.updateAll`
- added `MultiSet.where`
- added `MultiSet.isUnique`
- added `MultiSet.all`
- added `MultiSet.any`
- added `MultiSet.isEmpty`
