module SetHelper exposing (insertOrSingleton)

import Set exposing (Set)


insertOrSingleton : comparable -> Maybe (Set comparable) -> Set comparable
insertOrSingleton newValue maybeSet =
    case maybeSet of
        Just set ->
            Set.insert newValue set

        Nothing ->
            Set.singleton newValue
