module ListHelper exposing (filterSet, findDuplicates)

import Dict
import Dict.Extra as Dict
import DictHelper as Dict
import List.Extra as List
import Set exposing (Set)


{-| Filter a list by membership in a given set.
-}
filterSet : Set comparable -> List comparable -> List comparable
filterSet filter list =
    List.filter (\x -> Set.member x filter) list


{-| Returns a list of duplicate entries.
This function preserves the ordering.
-}
findDuplicates : List comparable -> List ( comparable, Int )
findDuplicates list =
    let
        counts =
            list
                |> Dict.groupBy identity
                |> Dict.map (\_ ls -> List.length ls)
                |> Dict.filter (\_ count -> count > 1)
    in
    List.filterMap (\x -> Dict.getWithKey x counts) list |> List.unique
