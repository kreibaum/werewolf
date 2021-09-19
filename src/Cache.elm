port module Cache exposing (cachePort)

import Json.Encode as E


port cachePort : E.Value -> Cmd msg
