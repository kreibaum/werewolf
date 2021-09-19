module Types exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Model =
    { roles : List Role
    , customRoleNameRawText : String
    , customRoleActionsRawText : String
    , selected : Dict String CardInformation
    , players : List Player
    , playersRawText : String
    , openCard : Maybe String
    , openPlayer : Maybe String
    , phase : GamePhase
    , uiScale : Int
    }


type alias CardInformation =
    { count : Int
    , players : Set String

    -- Maps from a role action name to a player name
    -- Elm does not yet mark simple wrapper types as comparable.
    , targetPlayers : Dict String (Set String)
    }


{-| Custom type to make sure we don't mix up all our different types of strings.
-}
type RoleAction
    = RoleAction String


type alias Player =
    { name : String
    , participation : Participation
    , note : String
    }


{-| Yes, this type name isn't great but I don't know any other word that fits.
-}
type Participation
    = Alive
    | Dead


type GamePhase
    = Preparation
    | Night
    | Day


type Msg
    = NoOp
    | AddRoleButtonClick String
    | RemoveRoleButtonClick String
    | TypePlayerNames String
    | SelectCard String
    | CloseCard
    | SelectPlayer String
    | ClosePlayer
    | AssignPlayerToRole String String
    | RemovePlayerFromRole String String
    | TargetPlayer String RoleAction String
    | RemoveTargetPlayer String RoleAction String
    | KillPlayer String
    | RevivePlayer String
    | SetPhase GamePhase
    | IncreaseFontSize
    | DecreaseFontSize
    | UpdatePlayerNote Player
    | TypeCustomRoleName String
    | TypeCustomRoleActions String
    | AddRole Role


type alias Role =
    { name : String
    , target : List RoleActionConfig
    }


type alias RoleActionConfig =
    { name : RoleAction }
