# changelog

Continuing [indique/elm-keysdict](https://package.elm-lang.org/packages/indique/elm-keysdict/latest/).

## 1.0.0

- removed `KeysDict.toDict`
- removed `KeysDict.Uniqueness.violated`
- changed `KeysDict.enterBy { door, key }` to `at door key`
- changed `KeysDict.remove { door, key }` to `remove door key`

- moved `KeysDict.Uniqueness.Uniqueness` to `KeysDict.Uniqueness`
- moved `KeysDict.Uniqueness.unique` to `KeysDict.unique`
- renamed `KeysDict.foldHouses` to `fold`
- renamed `KeysDict.countHouses` to `size`
- renamed `KeysDict.houses` to `toList`
- renamed `KeysDict.enterableBy` to `promising`

- added `KeysDict.insertAll`
- added `KeysDict.update`
- added `KeysDict.where`
- added `KeysDict.dropWhere`
- added `KeysDict.isUnique`
- added `KeysDict.all`
- added `KeysDict.any`
- added `KeysDict.isEmpty`
