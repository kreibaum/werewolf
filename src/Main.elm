module Main exposing (main)

import Browser
import Components exposing (uiArray)
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerY, el, fill, fillPortion, padding, rgb255, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Platform exposing (Program)


type alias Model =
    { templates : List String
    , selected : Dict String Int
    , players : List String
    , playersRawText : String
    }


type Msg
    = NoOp
    | AddRoleButtonClick String
    | RemoveRoleButtonClick String
    | TypePlayerNames String


init : Model
init =
    { templates = [ "Werwolf", "Seherin", "Hexe", "Seelenretter", "Vampir", "Jäger" ]
    , selected = Dict.empty
    , players = [ "Ada", "Berd", "Carol", "Dave", "Esther", "Felix", "Greta" ]
    , playersRawText = ""
    }


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddRoleButtonClick name ->
            { model | selected = addCard name model.selected }

        NoOp ->
            model

        RemoveRoleButtonClick template ->
            { model | selected = removeCard template model.selected }

        TypePlayerNames rawText ->
            setPlayerNames rawText model


addCard : String -> Dict String Int -> Dict String Int
addCard template dict =
    Dict.update template (Maybe.withDefault 0 >> (\x -> x + 1) >> Just) dict


removeCard : String -> Dict String Int -> Dict String Int
removeCard template dict =
    let
        substract n =
            if n > 1 then
                Just (n - 1)

            else
                Nothing
    in
    Dict.update template (Maybe.withDefault 0 >> substract) dict


cardCount : Model -> Int
cardCount model =
    List.sum <| Dict.values model.selected



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


view : Model -> Html Msg
view model =
    Element.layout [ width fill ]
        (mainView model)


mainView : Model -> Element Msg
mainView model =
    Element.column [ spacing 20, padding 10, width fill ]
        [ gameSetupHeader model
        , addCardsView model.templates
        , roleList model
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
            Dict.values (Dict.map roleDescription model.selected)

        villagerCount =
            playerCount model - cardCount model

        additionalVillagers =
            roleDescription "Dorfbewohner" villagerCount

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


roleDescription : String -> Int -> Element Msg
roleDescription name count =
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
            [ spacing 5 ]
            [ text <| String.fromInt count, text name, removeCardButton name ]


removeCardButton : String -> Element Msg
removeCardButton template =
    el
        [ alignRight
        , Events.onClick (RemoveRoleButtonClick template)
        , Background.color (rgb255 255 200 200)
        ]
        (text "x")


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
