module Main exposing (main)

import Browser
import Cache exposing (Cache, readCache)
import Components exposing (uiArray)
import Dict exposing (Dict)
import DictHelper as Dict
import Element exposing (Color, Element, alignRight, alignTop, el, fill, fillPortion, height, padding, rgb255, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon as Icon
import FontAwesome.Regular as Regular
import FontAwesome.Solid as Solid
import FontAwesome.Styles
import Html exposing (Html)
import Json.Encode exposing (Value)
import List.Extra as List
import ListHelper as List
import Platform exposing (Program)
import Set exposing (Set)


type alias Model =
    { templates : List String
    , customRoles : List String
    , customRolesRawText : String
    , selected : Dict String CardInformation
    , players : List Player
    , playersRawText : String
    , openCard : Maybe String
    , openPlayer : Maybe String
    , phase : GamePhase
    , uiScale : Int
    }


templateList : Model -> List String
templateList model =
    model.templates ++ model.customRoles


type alias CardInformation =
    { count : Int
    , players : Set String
    , targetPlayers : Set String
    }


newCard : CardInformation
newCard =
    { count = 1
    , players = Set.empty
    , targetPlayers = Set.empty
    }


type alias Player =
    { name : String
    , participation : Participation
    , note : String
    }


newPlayer : String -> Player
newPlayer name =
    { name = name
    , participation = Alive
    , note = ""
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
    = AddRoleButtonClick String
    | RemoveRoleButtonClick String
    | TypePlayerNames String
    | TypeCustomRoles String
    | SelectCard String
    | CloseCard
    | SelectPlayer String
    | ClosePlayer
    | AssignPlayerToRole String String
    | RemovePlayerFromRole String String
    | TargetPlayer String String
    | RemoveTargetPlayer String String
    | KillPlayer String
    | RevivePlayer String
    | SetPhase GamePhase
    | IncreaseFontSize
    | DecreaseFontSize
    | UpdatePlayerNote Player


init : Value -> ( Model, Cmd msg )
init flags =
    let
        initialModel =
            case readCache flags of
                Ok cache ->
                    applyCache cache defaultModel

                Err _ ->
                    defaultModel
    in
    ( initialModel
    , Cmd.none
    )


defaultModel : Model
defaultModel =
    { templates = [ "Amor", "Werwolf", "Seherin", "Hexe", "Seelenretter", "Vampir", "Jäger", "Fauli", "Mathematiker", "Gärtner" ]
    , customRoles = []
    , customRolesRawText = ""
    , selected = Dict.empty
    , players =
        [ "Ada", "Bert", "Carol", "Dave", "Esther", "Felix", "Greta" ]
            |> List.map newPlayer
    , playersRawText = ""
    , openCard = Nothing
    , openPlayer = Nothing
    , phase = Preparation
    , uiScale = 2
    }


applyCache : Cache -> Model -> Model
applyCache cache model =
    { model | players = cache.names |> List.map newPlayer }


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , update = updateWithCommand
        , view = view
        , subscriptions = \_ -> Sub.none
        }


updateWithCommand : Msg -> Model -> ( Model, Cmd msg )
updateWithCommand msg model =
    let
        newModel =
            update msg model

        cacheCommand =
            { names = newModel.players |> List.map .name }
                |> Cache.writeCache
                |> Cache.cachePort
    in
    ( newModel, cacheCommand )


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddRoleButtonClick name ->
            { model | selected = addCard name model.selected }

        RemoveRoleButtonClick template ->
            { model | selected = removeCard template model.selected }

        TypePlayerNames rawText ->
            setPlayerNames rawText model

        TypeCustomRoles rawText ->
            setCustomRoles rawText model

        SelectCard identifier ->
            selectCard identifier model

        CloseCard ->
            { model | openCard = Nothing }

        SelectPlayer name ->
            { model | openPlayer = Just name }

        ClosePlayer ->
            { model | openPlayer = Nothing }

        AssignPlayerToRole cardName playerName ->
            assignPlayerToRole cardName playerName model

        RemovePlayerFromRole cardName playerName ->
            removePlayerFromRole cardName playerName model

        TargetPlayer cardName playerName ->
            targetPlayer cardName playerName model

        RemoveTargetPlayer cardName playerName ->
            removeTargetPlayer cardName playerName model

        KillPlayer name ->
            { model | players = List.updateIf (\p -> p.name == name) killPlayer model.players }

        RevivePlayer name ->
            { model | players = List.updateIf (\p -> p.name == name) revivePlayer model.players }

        SetPhase newPhase ->
            { model | phase = newPhase }

        IncreaseFontSize ->
            { model | uiScale = model.uiScale + 1 }

        DecreaseFontSize ->
            { model | uiScale = model.uiScale - 1 }

        UpdatePlayerNote player ->
            { model | players = List.setIf (\p -> p.name == player.name) player model.players }


killPlayer : Player -> Player
killPlayer p =
    { p | participation = Dead }


revivePlayer : Player -> Player
revivePlayer p =
    { p | participation = Alive }


addCard : String -> Dict String CardInformation -> Dict String CardInformation
addCard template dict =
    let
        closure x =
            case x of
                Just a ->
                    Just { a | count = a.count + 1 }

                Nothing ->
                    Just newCard
    in
    Dict.update template closure dict


removeCard : String -> Dict String CardInformation -> Dict String CardInformation
removeCard template dict =
    let
        substract : CardInformation -> Maybe CardInformation
        substract cardInfo =
            if cardInfo.count > 1 then
                Just { cardInfo | count = cardInfo.count - 1 }

            else
                Nothing
    in
    Dict.update template (Maybe.andThen substract) dict


cardCount : Model -> Int
cardCount model =
    model.selected
        |> Dict.filter (\name _ -> List.member name (templateList model))
        |> Dict.values
        |> List.map (\x -> x.count)
        |> List.sum


setPlayerNames : String -> Model -> Model
setPlayerNames rawText model =
    let
        names =
            parsePlayerNames rawText

        newPlayers =
            if List.isEmpty names then
                model.players

            else
                names |> List.map newPlayer
    in
    { model | players = newPlayers, playersRawText = rawText }


parsePlayerNames : String -> List String
parsePlayerNames rawText =
    rawText
        |> String.split ","
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)


{-| Note that this is simpler than `setPlayerNames` as we allow an empty list.
-}
setCustomRoles : String -> Model -> Model
setCustomRoles rawText model =
    { model | customRoles = parsePlayerNames rawText, customRolesRawText = rawText }


selectCard : String -> Model -> Model
selectCard identifier model =
    if openRolesAllowed model.phase then
        { model | openCard = Just identifier }

    else
        model


playerCount : Model -> Int
playerCount model =
    List.length model.players


assignPlayerToRole : String -> String -> Model -> Model
assignPlayerToRole cardName playerName model =
    { model | selected = Dict.update cardName (Maybe.map <| assignPlayer playerName) model.selected }


assignPlayer : String -> CardInformation -> CardInformation
assignPlayer name cardInfo =
    { cardInfo | players = Set.insert name cardInfo.players }


removePlayerFromRole : String -> String -> Model -> Model
removePlayerFromRole cardName playerName model =
    { model | selected = Dict.update cardName (Maybe.map <| removePlayer playerName) model.selected }


removePlayer : String -> CardInformation -> CardInformation
removePlayer name cardInfo =
    { cardInfo | players = Set.remove name cardInfo.players }


targetPlayer : String -> String -> Model -> Model
targetPlayer cardName playerName model =
    { model | selected = Dict.update cardName (Maybe.map <| targetOnePlayer playerName) model.selected }


targetOnePlayer : String -> CardInformation -> CardInformation
targetOnePlayer name cardInfo =
    { cardInfo | targetPlayers = Set.insert name cardInfo.targetPlayers }


removeTargetPlayer : String -> String -> Model -> Model
removeTargetPlayer cardName playerName model =
    { model | selected = Dict.update cardName (Maybe.map <| removeTargetOnePlayer playerName) model.selected }


removeTargetOnePlayer : String -> CardInformation -> CardInformation
removeTargetOnePlayer name cardInfo =
    { cardInfo | targetPlayers = Set.remove name cardInfo.targetPlayers }



-------------------------------
-- Here starts the View Code --
-------------------------------


fontScale : Model -> Int -> Int
fontScale model factor =
    24 * 1.1 ^ toFloat (factor + model.uiScale) |> round


targetColor : Color
targetColor =
    rgb255 255 255 150


roleColor : Color
roleColor =
    rgb255 200 200 255


lightShade : Color
lightShade =
    rgb255 240 240 240


focusShade : Color
focusShade =
    rgb255 230 230 230


hardBorderColor : Color
hardBorderColor =
    rgb255 100 100 100


textColor : Color
textColor =
    rgb255 0 0 0


secondaryTextColor : Color
secondaryTextColor =
    rgb255 120 120 120


listBackground : Int -> Color
listBackground i =
    if modBy 2 i == 0 then
        rgb255 240 240 240

    else
        rgb255 220 220 220


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , Font.size <| fontScale model 0
        , Font.color textColor
        ]
        (mainView model)


mainView : Model -> Element Msg
mainView model =
    Element.column [ width fill ]
        [ Element.html FontAwesome.Styles.css
        , phaseHeader model
        , phaseView model
        ]


phaseView : Model -> Element Msg
phaseView model =
    Element.column [ spacing 20, width fill ]
        [ gameSetupHeader model
        , roleList model
        , playerSummary model
        ]


phaseHeader : Model -> Element Msg
phaseHeader model =
    Element.row
        [ width fill ]
        [ phaseTab model Preparation
        , phaseTab model Night
        , phaseTab model Day
        ]


phaseTab : Model -> GamePhase -> Element Msg
phaseTab model phase =
    let
        caption =
            case phase of
                Preparation ->
                    "Vorbereitung"

                Night ->
                    "Nacht"

                Day ->
                    "Tag"

        icon =
            case phase of
                Preparation ->
                    Solid.cogs

                Night ->
                    Solid.moon

                Day ->
                    Solid.sun

        isActive =
            phase == model.phase

        portion =
            if isActive then
                3

            else
                2

        background =
            if isActive then
                focusShade

            else
                lightShade
    in
    Element.column
        [ width (fillPortion portion)
        , Background.color background
        , padding 10
        , spacing 10
        , Events.onClick (SetPhase phase)
        ]
        [ el [ Element.centerX, Font.size <| fontScale model 4 ] <| Element.html <| Icon.view icon
        , el [ Element.centerX ] (text caption)
        ]


gameSetupHeader : Model -> Element Msg
gameSetupHeader model =
    if model.phase == Preparation then
        Element.column [ width fill, spacing 10 ]
            [ setupTitle model
            , playerCountEditBox model
            , playerDuplicationWarning model
            , customRolesEditBox model
            , addCardsView (templateList model)
            ]

    else
        Element.none


setupTitle : Model -> Element Msg
setupTitle model =
    Element.row
        [ width fill, spacing 10, padding 10 ]
        [ el [ Font.size <| fontScale model 6 ] (text "Werwolf Klemmbrett")
        , zoomButton model [ Events.onClick IncreaseFontSize ] (Element.html <| Icon.view Solid.searchPlus)
        , zoomButton model [ Events.onClick DecreaseFontSize ] (Element.html <| Icon.view Solid.searchMinus)
        ]


zoomButton : Model -> List (Element.Attribute msg) -> Element msg -> Element msg
zoomButton model styles caption =
    el
        (List.append
            [ Background.color lightShade
            , alignRight
            , alignTop
            , Font.size <| fontScale model -2
            , Border.rounded 5
            , padding 10
            ]
            styles
        )
        caption


playerCountEditBox : Model -> Element Msg
playerCountEditBox model =
    Element.el [ padding 10, width fill ]
        (Input.text []
            { onChange = TypePlayerNames
            , text = model.playersRawText
            , placeholder = Just <| Input.placeholder [] <| text <| String.join ", " <| List.map .name model.players
            , label = Input.labelAbove [] (text "Mitspieler: ")
            }
        )


customRolesEditBox : Model -> Element Msg
customRolesEditBox model =
    let
        placeholderText =
            if List.isEmpty model.customRoles then
                "Eigene Rollen eingeben"

            else
                String.join ", " model.customRoles
    in
    Element.el [ padding 10, width fill ]
        (Input.text []
            { onChange = TypeCustomRoles
            , text = model.customRolesRawText
            , placeholder = Just <| Input.placeholder [] <| text placeholderText
            , label = Input.labelAbove [] (text "Rollen: ")
            }
        )


playerDuplicationWarning : Model -> Element msg
playerDuplicationWarning model =
    let
        duplicates : List String
        duplicates =
            model.players
                |> List.map .name
                |> List.findDuplicates
                |> List.map (\( name, _ ) -> name)

        duplicateNames =
            String.join ", " duplicates
    in
    if List.isEmpty duplicates then
        Element.none

    else
        text <| "Jeder Name darf nur einmal vorkommen, aber " ++ duplicateNames ++ " ist mehrfach angegeben."


addCardsView : List String -> Element Msg
addCardsView templates =
    Element.column [ width fill, spacing 5 ]
        [ Element.el [ padding 10, width fill ] (text "Sonderrollen hinzufügen:")
        , buttonArray templates
        ]


buttonArray : List String -> Element Msg
buttonArray templates =
    uiArray 3
        (List.indexedMap roleButton templates)


roleButton : Int -> String -> Element Msg
roleButton i name =
    el
        [ padding 15
        , width fill
        , Events.onClick (AddRoleButtonClick name)
        , Background.color (listBackground i)
        ]
        (text name)


roleList : Model -> Element Msg
roleList model =
    let
        specialCards =
            templateList model
                |> List.filterMap (\t -> Dict.getWithKey t model.selected)
                |> List.indexedMap (roleDescription model)

        playerCountText =
            String.join ""
                [ "(Spieler: "
                , String.fromInt (playerCount model)
                , ", Rollen: "
                , String.fromInt (cardCount model)
                , ")"
                ]
    in
    Element.column [ width fill ] <|
        (Element.row [ padding 10, width fill, spacing 5 ]
            [ text "Rollenübersicht:"
            , Element.el [ Font.color secondaryTextColor ] (text playerCountText)
            ]
            :: specialCards
        )


roleDescription : Model -> Int -> ( String, CardInformation ) -> Element Msg
roleDescription model i ( name, count ) =
    if model.openCard == Just name && openRolesAllowed model.phase then
        cardOpenView model i name count

    else
        roleDescriptionClosed model i name count


openRolesAllowed : GamePhase -> Bool
openRolesAllowed phase =
    case phase of
        Preparation ->
            True

        Night ->
            True

        Day ->
            False


roleDescriptionClosed : Model -> Int -> String -> CardInformation -> Element Msg
roleDescriptionClosed model i name cardInfo =
    el
        [ width fill
        , Background.color (listBackground i)
        ]
    <|
        Element.row
            [ spacing 5, width fill ]
            [ roleHeader model name cardInfo
            , removeCardButton model name
            ]


roleHeader : Model -> String -> CardInformation -> Element Msg
roleHeader model name cardInfo =
    let
        informationText =
            if openRolesAllowed model.phase then
                playerBadgeList model cardInfo

            else
                Element.none
    in
    Element.row
        [ spacing 5
        , width fill
        , padding 15
        , Events.onClick (SelectCard name)
        ]
        [ text <| String.fromInt cardInfo.count
        , text name
        , informationText
        ]


removeCardButton : Model -> String -> Element Msg
removeCardButton model template =
    if model.phase == Preparation then
        el
            [ alignRight
            , Events.onClick (RemoveRoleButtonClick template)
            , Font.color (rgb255 200 0 0)
            , height fill
            , padding 10
            ]
            removeCardIcon

    else
        Element.none


removeCardIcon : Element msg
removeCardIcon =
    Element.html <| Icon.view Solid.minusSquare


cardOpenView : Model -> Int -> String -> CardInformation -> Element Msg
cardOpenView model i name cardInfo =
    Element.column
        [ width fill
        , Background.color (listBackground i)
        , Border.color hardBorderColor
        , Border.width 1
        ]
        [ cardHeaderOpen model name cardInfo
        , cardContent model name cardInfo
        ]


cardHeaderOpen : Model -> String -> CardInformation -> Element Msg
cardHeaderOpen model name cardInfo =
    Element.row
        [ spacing 5, width fill ]
        [ roleHeaderOpen name cardInfo, removeCardButton model name ]


roleHeaderOpen : String -> CardInformation -> Element Msg
roleHeaderOpen name cardInfo =
    Element.row
        [ spacing 5
        , width fill
        , padding 15
        , Events.onClick CloseCard
        ]
        [ text <| String.fromInt cardInfo.count
        , text name
        ]


cardContent : Model -> String -> CardInformation -> Element Msg
cardContent model name cardInfo =
    Element.column
        [ width fill
        , height fill
        , padding 15
        , spacing 10
        ]
        [ text "Spielerauswahl"
        , playerCardSelection model name cardInfo
        , text "Zielauswahl"
        , cardTargetSelection model name cardInfo
        ]


playerCardSelection : Model -> String -> CardInformation -> Element Msg
playerCardSelection model name cardInfo =
    model.players
        |> List.map (playerSelector name cardInfo)
        |> Element.wrappedRow [ spacing 5 ]


playerSelector : String -> CardInformation -> Player -> Element Msg
playerSelector cardName cardInfo player =
    let
        isAlreadySelected =
            Set.member player.name cardInfo.players

        event =
            if isAlreadySelected then
                RemovePlayerFromRole cardName player.name

            else
                AssignPlayerToRole cardName player.name
    in
    onOffButton roleColor isAlreadySelected (playerNameText player) event


cardTargetSelection : Model -> String -> CardInformation -> Element Msg
cardTargetSelection model name cardInfo =
    model.players
        |> List.map (targetSelector name cardInfo)
        |> Element.wrappedRow [ spacing 5 ]


targetSelector : String -> CardInformation -> Player -> Element Msg
targetSelector cardName cardInfo player =
    let
        isAlreadySelected =
            Set.member player.name cardInfo.targetPlayers

        event =
            if isAlreadySelected then
                RemoveTargetPlayer cardName player.name

            else
                TargetPlayer cardName player.name
    in
    onOffButton targetColor isAlreadySelected (playerNameText player) event


onOffButton : Color -> Bool -> Element Never -> Msg -> Element Msg
onOffButton color isSelected caption event =
    let
        selectionStyle =
            if isSelected then
                [ Background.color color ]

            else
                [ Border.color color
                , Border.width 1
                ]

        styles =
            List.append selectionStyle
                [ padding 7
                , Events.onClick event
                ]
    in
    el styles (Element.map never caption)


playerBadgeList : Model -> CardInformation -> Element msg
playerBadgeList model cardInfo =
    let
        selectedPlayers =
            model.players
                |> List.filter (\p -> Set.member p.name cardInfo.players)
                |> List.map (playerNameText >> roleBadge model)

        targetPlayers =
            model.players
                |> List.filter (\p -> Set.member p.name cardInfo.targetPlayers)
                |> List.map (playerNameText >> targetBadge model)

        entries =
            if List.isEmpty targetPlayers then
                selectedPlayers

            else
                List.append selectedPlayers (text "mit Ziel" :: targetPlayers)
    in
    Element.wrappedRow [ spacing 5, width fill ] <| entries


badge : Model -> List (Element.Attribute msg) -> Element msg -> Element msg
badge model styles caption =
    el
        (List.append
            [ Font.size <| fontScale model -4
            , Border.rounded 3
            , padding 4
            ]
            styles
        )
        caption


roleBadge : Model -> Element msg -> Element msg
roleBadge model caption =
    badge model [ Background.color roleColor ] caption


targetBadge : Model -> Element msg -> Element msg
targetBadge model caption =
    badge model [ Background.color targetColor ] caption


playerSummary : Model -> Element Msg
playerSummary model =
    Element.column
        [ width fill ]
        (Element.el [ padding 10, width fill ]
            (text "Spielerübersicht:")
            :: List.indexedMap (playerDetails model) model.players
        )


playerDetails : Model -> Int -> Player -> Element Msg
playerDetails model i player =
    if model.openPlayer == Just player.name then
        openPlayer model i player

    else
        closedPlayer model i player


openPlayer : Model -> Int -> Player -> Element Msg
openPlayer model i player =
    Element.column
        [ width fill
        , spacing 5
        , Border.color hardBorderColor
        , Border.width 1
        , Background.color (listBackground i)
        ]
        [ openPlayerHeader model player, openPlayerBody player ]


openPlayerHeader : Model -> Player -> Element Msg
openPlayerHeader model player =
    Element.row
        [ width fill
        , spacing 5
        , Events.onClick ClosePlayer
        , padding 15
        ]
        (playerHeader model player)


openPlayerBody : Player -> Element Msg
openPlayerBody player =
    Element.column
        [ width fill
        , spacing 10
        , padding 15
        ]
        [ deadOrAliveSetting player
        , noteEditor player
        ]


deadOrAliveSetting : Player -> Element Msg
deadOrAliveSetting player =
    case player.participation of
        Dead ->
            el [ Events.onClick (RevivePlayer player.name) ] (text "Wiederbeleben")

        Alive ->
            el [ Events.onClick (KillPlayer player.name) ] (text "Töten")


noteEditor : Player -> Element Msg
noteEditor player =
    Input.text []
        { onChange = \newNote -> UpdatePlayerNote { player | note = newNote }
        , text = player.note
        , placeholder = Just <| Input.placeholder [] <| text "Hier kannst du eine Notiz eingeben."
        , label = Input.labelAbove [] (text "Notiz: ")
        }


closedPlayer : Model -> Int -> Player -> Element Msg
closedPlayer model i player =
    Element.row
        [ width fill
        , spacing 5
        , padding 15
        , Background.color (listBackground i)
        , Events.onClick (SelectPlayer player.name)
        ]
        (playerHeader model player)


playerHeader : Model -> Player -> List (Element msg)
playerHeader model player =
    let
        cards =
            templateList model
                |> List.filterSet (cardsByPlayer model player.name)
                |> List.map (text >> roleBadge model)

        cardDisplay =
            if List.isEmpty cards then
                []

            else
                text "ist selbst" :: cards

        targetDisplayTextGlue =
            if List.isEmpty cardDisplay then
                "ist Ziel von"

            else
                "und ist Ziel von"

        targeting =
            templateList model
                |> List.filterSet (targetingCardsByPlayer model player.name)
                |> List.map (text >> targetBadge model)

        targetingDisplay =
            if List.isEmpty targeting then
                []

            else
                text targetDisplayTextGlue :: targeting

        informationText =
            if openRolesAllowed model.phase then
                cardDisplay ++ targetingDisplay ++ [ playerNote player ]

            else
                []
    in
    playerNameText player :: informationText


playerNote : Player -> Element msg
playerNote player =
    if String.isEmpty player.note then
        Element.none

    else
        Element.row
            [ Font.color secondaryTextColor
            , spacing 5
            , alignRight
            , Font.italic
            ]
            [ noteIcon, text player.note ]


noteIcon : Element msg
noteIcon =
    el [] <| Element.html <| Icon.view Regular.stickyNote


playerNameText : Player -> Element msg
playerNameText player =
    let
        style =
            case player.participation of
                Dead ->
                    [ Font.strike, spacing 5 ]

                Alive ->
                    [ spacing 5 ]

        content =
            case player.participation of
                Dead ->
                    [ text player.name, deadIcon ]

                Alive ->
                    [ text player.name ]
    in
    Element.row style content


deadIcon : Element msg
deadIcon =
    el [] <| Element.html <| Icon.view Solid.skullCrossbones


cardsByPlayer : Model -> String -> Set String
cardsByPlayer model name =
    model.selected
        |> Dict.filter (\_ cardInfo -> Set.member name cardInfo.players)
        |> Dict.keys
        |> Set.fromList


targetingCardsByPlayer : Model -> String -> Set String
targetingCardsByPlayer model name =
    model.selected
        |> Dict.filter (\_ cardInfo -> Set.member name cardInfo.targetPlayers)
        |> Dict.keys
        |> Set.fromList
