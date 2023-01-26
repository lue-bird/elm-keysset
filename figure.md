Expected:
```elm
Typed Checked tags Public
    { a | record : Typed Checked KeyTag Public { index : N (In (Up x To indexPlusX) (Up x To indexPlusX)), toKey : Mapping element elementToKeyTag key, keyOrder : Ordering key elementKeyComparingTag }
    -> keysFedThoMaybeStillHungry
    , index : N (In (Up x To indexPlusX) (Up x To indexPlusX))
    , toArray : keysComplete -> ArraySized ((element, element) -> Order) (In (Up (N0) To keyCount) (Up (N0) To keyCount))
    }
```
Found:
```elm
KeysInProgress
    element
    keyCount
    (Up x To indexPlusX)
    tags
    keysComplete
    (Key element (Up x To indexPlusX) key -> keysFedThoMaybeStillHungry)
```