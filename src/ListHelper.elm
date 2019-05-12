module ListHelper exposing (filterSet)

import Set exposing (Set)


filterSet : Set comparable -> List comparable -> List comparable
filterSet filter list =
    List.filter (\x -> Set.member x filter) list
