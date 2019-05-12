module Main exposing (main)

import Browser
import Components exposing (uiArray)
import Dict exposing (Dict)
import DictHelper as Dict
import Element exposing (Color, Element, alignRight, el, fill, height, padding, rgb255, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import ListHelper as List
import Platform exposing (Program)
import Set exposing (Set)


type alias Model =
    { templates : List String
    , selected : Dict String CardInformation
    , players : List String
    , playersRawText : String
    , openCard : Maybe String
    }


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


type Msg
    = AddRoleButtonClick String
    | RemoveRoleButtonClick String
    | TypePlayerNames String
    | SelectCard String
    | CloseCard
    | AssignPlayerToRole String String
    | RemovePlayerFromRole String String
    | TargetPlayer String String
    | RemoveTargetPlayer String String


init : Model
init =
    { templates = [ "Werwolf", "Seherin", "Hexe", "Seelenretter", "Vampir", "Jäger", "Amor", "Fauli", "Mathematiker", "Gärtner" ]
    , selected = Dict.empty
    , players = [ "Ada", "Bert", "Carol", "Dave", "Esther", "Felix", "Greta" ]
    , playersRawText = ""
    , openCard = Nothing
    }


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddRoleButtonClick name ->
            { model | selected = addCard name model.selected }

        RemoveRoleButtonClick template ->
            { model | selected = removeCard template model.selected }

        TypePlayerNames rawText ->
            setPlayerNames rawText model

        SelectCard identifier ->
            { model | openCard = Just identifier }

        CloseCard ->
            { model | openCard = Nothing }

        AssignPlayerToRole cardName playerName ->
            assignPlayerToRole cardName playerName model

        RemovePlayerFromRole cardName playerName ->
            removePlayerFromRole cardName playerName model

        TargetPlayer cardName playerName ->
            targetPlayer cardName playerName model

        RemoveTargetPlayer cardName playerName ->
            removeTargetPlayer cardName playerName model


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
    List.sum <| List.map (\x -> x.count) <| Dict.values model.selected



-- setPlayerCount : String -> Model -> Model
-- setPlayerCount rawText model =
--     let
--         newPlayerCount =
--             String.toInt rawText |> Maybe.withDefault ()
--     in
--     { model | playerCount = newPlayerCount, playerCountRawText = rawText }


setPlayerNames : String -> Model -> Model
setPlayerNames rawText model =
    let
        names =
            parsePlayerNames rawText

        newPlayers =
            if List.length names > 0 then
                names

            else
                model.players
    in
    { model | players = newPlayers, playersRawText = rawText }


parsePlayerNames : String -> List String
parsePlayerNames rawText =
    rawText
        |> String.split ","
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)


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


fontScale : Int -> Int
fontScale factor =
    let
        base =
            16
    in
    if factor > 0 then
        base * 1.25 ^ toFloat (factor - 1) |> round

    else if factor == 0 then
        base

    else
        -- negative factor
        base * 1.25 ^ toFloat factor |> round


targetColor : Color
targetColor =
    rgb255 255 255 150


roleColor : Color
roleColor =
    rgb255 200 200 255


view : Model -> Html Msg
view model =
    Element.layout [ width fill, Font.size <| fontScale 1 ]
        (mainView model)


mainView : Model -> Element Msg
mainView model =
    Element.column [ spacing 20, padding 10, width fill ]
        [ gameSetupHeader model
        , addCardsView model.templates
        , roleList model
        , playerSummary model
        ]


gameSetupHeader : Model -> Element Msg
gameSetupHeader model =
    Element.row [ width fill ]
        [ playerCountEditBox model ]


playerCountEditBox : Model -> Element Msg
playerCountEditBox model =
    Input.text []
        { onChange = TypePlayerNames
        , text = model.playersRawText
        , placeholder = Just <| Input.placeholder [] <| text <| String.join ", " model.players
        , label = Input.labelAbove [] (text "Mitspieler: ")
        }


addCardsView : List String -> Element Msg
addCardsView templates =
    Element.column [ width fill, spacing 5 ]
        [ text "Sonderrollen hinzufügen:"
        , buttonArray templates
        ]


buttonArray : List String -> Element Msg
buttonArray templates =
    uiArray 4
        (List.map roleButton templates)


roleButton : String -> Element Msg
roleButton name =
    el
        [ Background.color (rgb255 200 200 200)
        , Font.color (rgb255 0 0 0)
        , Border.rounded 5
        , padding 10
        , width fill
        , Events.onClick (AddRoleButtonClick name)
        ]
        (text name)


roleList : Model -> Element Msg
roleList model =
    let
        specialCards =
            model.templates
                |> List.filterMap (\t -> Dict.getWithKey t model.selected)
                |> List.map (roleDescription model)

        villagerCount =
            playerCount model - cardCount model

        additionalVillagers =
            roleDescriptionClosed model "Dorfbewohner" { newCard | count = villagerCount }

        allCards =
            if villagerCount < 0 then
                List.append specialCards [ playerLimitBreached (playerCount model) (cardCount model) ]

            else if villagerCount == 0 then
                specialCards

            else
                List.append specialCards [ additionalVillagers ]
    in
    Element.column [ spacing 10, width fill ] <|
        allCards


roleDescription : Model -> ( String, CardInformation ) -> Element Msg
roleDescription model ( name, count ) =
    if model.openCard == Just name then
        cardOpenView model name count

    else
        roleDescriptionClosed model name count


roleDescriptionClosed : Model -> String -> CardInformation -> Element Msg
roleDescriptionClosed model name cardInfo =
    el
        [ Font.color (rgb255 0 0 0)
        , Border.rounded 5
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , padding 10
        , width fill
        ]
    <|
        Element.row
            [ spacing 5, width fill ]
            [ text <| String.fromInt cardInfo.count
            , roleDescriptionLabelClosed name
            , playerBadgeList model cardInfo
            , removeCardButton name
            ]


roleDescriptionLabelClosed : String -> Element Msg
roleDescriptionLabelClosed name =
    el [ Events.onClick (SelectCard name) ] (text name)


roleDescriptionLabelOpened : String -> Element Msg
roleDescriptionLabelOpened name =
    el [ Events.onClick CloseCard ] (text name)


removeCardButton : String -> Element Msg
removeCardButton template =
    el
        [ alignRight
        , Events.onClick (RemoveRoleButtonClick template)
        , Background.color (rgb255 255 200 200)
        ]
        (text "x")


cardOpenView : Model -> String -> CardInformation -> Element Msg
cardOpenView model name cardInfo =
    Element.column
        [ Border.rounded 5
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , width fill
        ]
        [ cardHeaderOpen model name cardInfo
        , cardContent model name cardInfo
        ]


cardHeaderOpen : Model -> String -> CardInformation -> Element Msg
cardHeaderOpen model name cardInfo =
    el
        [ Font.color (rgb255 0 0 0)
        , Font.bold
        , padding 10
        , width fill
        ]
    <|
        Element.row
            [ spacing 5, width fill ]
            [ text <| String.fromInt cardInfo.count, roleDescriptionLabelOpened name, removeCardButton name ]


cardContent : Model -> String -> CardInformation -> Element Msg
cardContent model name cardInfo =
    Element.column
        [ Background.color (rgb255 230 230 230)
        , width fill
        , height fill
        , padding 10
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


playerSelector : String -> CardInformation -> String -> Element Msg
playerSelector cardName cardInfo playerName =
    let
        isAlreadySelected =
            Set.member playerName cardInfo.players

        event =
            if isAlreadySelected then
                RemovePlayerFromRole cardName playerName

            else
                AssignPlayerToRole cardName playerName
    in
    onOffButton isAlreadySelected playerName event


cardTargetSelection : Model -> String -> CardInformation -> Element Msg
cardTargetSelection model name cardInfo =
    model.players
        |> List.map (targetSelector name cardInfo)
        |> Element.wrappedRow [ spacing 5 ]


targetSelector : String -> CardInformation -> String -> Element Msg
targetSelector cardName cardInfo playerName =
    let
        isAlreadySelected =
            Set.member playerName cardInfo.targetPlayers

        event =
            if isAlreadySelected then
                RemoveTargetPlayer cardName playerName

            else
                TargetPlayer cardName playerName
    in
    onOffButton isAlreadySelected playerName event


onOffButton : Bool -> String -> Msg -> Element Msg
onOffButton isSelected caption event =
    let
        selectionStyle =
            if isSelected then
                [ Background.color (rgb255 200 200 200) ]

            else
                [ Border.color (rgb255 200 200 200)
                , Border.width 1
                ]

        styles =
            List.append selectionStyle
                [ Border.rounded 5
                , padding 7
                , Events.onClick event
                ]
    in
    el styles (text caption)


playerBadgeList : Model -> CardInformation -> Element msg
playerBadgeList model cardInfo =
    let
        selectedPlayers =
            model.players
                |> List.filterSet cardInfo.players
                |> List.map roleBadge

        targetPlayers =
            model.players
                |> List.filterSet cardInfo.targetPlayers
                |> List.map targetBadge

        entries =
            if List.isEmpty targetPlayers then
                selectedPlayers

            else
                List.append selectedPlayers (text "mit Ziel" :: targetPlayers)
    in
    Element.wrappedRow [ spacing 5, width fill ] <| entries


badge : Element.Color -> String -> Element msg
badge color caption =
    el
        [ Font.size <| fontScale -1
        , Border.rounded 3
        , padding 4
        , Background.color color
        ]
        (text caption)


roleBadge : String -> Element msg
roleBadge caption =
    badge roleColor caption


targetBadge : String -> Element msg
targetBadge caption =
    badge targetColor caption


playerLimitBreached : Int -> Int -> Element msg
playerLimitBreached expected actual =
    el
        [ Font.color (rgb255 0 0 0)
        , Border.rounded 5
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , padding 10
        , width fill
        ]
        (text <|
            "Achtung, du hast "
                ++ String.fromInt actual
                ++ " Karten auf "
                ++ String.fromInt expected
                ++ " Spieler verteilt!"
        )


playerSummary : Model -> Element Msg
playerSummary model =
    Element.column
        [ width fill, spacing 5 ]
    <|
        text "Spielerübersicht:"
            :: List.map (playerDetails model) model.players


playerDetails : Model -> String -> Element Msg
playerDetails model name =
    let
        cards =
            model.templates
                |> List.filterSet (cardsByPlayer model name)
                |> List.map roleBadge

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
            model.templates
                |> List.filterSet (targetingCardsByPlayer model name)
                |> List.map targetBadge

        targetingDisplay =
            if List.isEmpty targeting then
                []

            else
                text targetDisplayTextGlue :: targeting
    in
    Element.row
        [ width fill
        , spacing 5
        , Border.rounded 5
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , padding 10
        ]
    <|
        List.append (text name :: cardDisplay) targetingDisplay


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
