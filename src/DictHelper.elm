module DictHelper exposing (getWithKey)

import Dict exposing (Dict)


getWithKey : comparable -> Dict comparable v -> Maybe ( comparable, v )
getWithKey key dict =
    Dict.get key dict
        |> Maybe.map (\v -> ( key, v ))
