module Components exposing (uiArray)

import Element exposing (Element, el, fill, spacing, width)
import List.Extra as List


{-| Ui Array component
-}
uiArray : Int -> List (Element msg) -> Element msg
uiArray columnCount elements =
    List.greedyGroupsOf columnCount (List.map uiArrayEntry elements)
        |> List.map (padd columnCount uiArraySpacer)
        |> List.map uiArrayRow
        |> Element.column [ spacing 10, width fill ]


padd : Int -> a -> List a -> List a
padd minWidth filler list =
    if minWidth > List.length list then
        List.append list (List.repeat (minWidth - List.length list) filler)

    else
        list


uiArraySpacer : Element msg
uiArraySpacer =
    el [ width fill ] Element.none


uiArrayRow : List (Element msg) -> Element msg
uiArrayRow entries =
    Element.row [ spacing 10, width fill ] entries


uiArrayEntry : Element msg -> Element msg
uiArrayEntry wrappedElement =
    el [ width fill ] wrappedElement
