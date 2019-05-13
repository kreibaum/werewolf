module DictHelper exposing (getWithKey)

import Dict exposing (Dict)


{-| Get the (key, value) pair associated with a key. If the key
is not found, return `Nothing` instead of `(key, Nothing)`.
-}
getWithKey : comparable -> Dict comparable v -> Maybe ( comparable, v )
getWithKey key dict =
    Dict.get key dict
        |> Maybe.map (\v -> ( key, v ))
