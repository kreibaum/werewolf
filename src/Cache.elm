port module Cache exposing (Cache, cachePort, readCache, writeCache)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


port cachePort : E.Value -> Cmd msg


{-| This cache module takes care of reading and writing local storage.
It exposes the record type `Cache` but does not tranfer any data to / from the model.
-}
type alias Cache =
    { names : List String }


readCache : D.Value -> Result D.Error Cache
readCache value =
    D.decodeValue decodeCache value


writeCache : Cache -> E.Value
writeCache cache =
    E.object
        [ ( "names", E.list E.string cache.names )
        ]


decodeCache : Decoder Cache
decodeCache =
    D.map Cache
        (D.field "names" (D.list D.string))
