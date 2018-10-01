module View.Anagram exposing (view)

import Array
import Browser
import Data.Anagram exposing (AnagramResponse)
import Element exposing (..)
import Element.Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JD
import List.Extra as List
import Task


view : AnagramResponse -> Element msg
view anagram =
    let
        groups =
            toGroups anagram.fullWord anagram.withBlanks
    in
    row [] <| List.map toElement groups



{- FLEISCH FL_I_CH => [(FL,false),(E, True),(I, false),(S,true),(CH,false)]
   FLEISCH __EISCH => [(FL, True), (EISCH, False)]
   FLEISCH FLEIS__  => [(FLEIS,False), (CH, True)]
-}


black =
    rgb 0 0 0


red =
    rgb 1 0 0


elementColor : Bool -> Element.Color
elementColor isBlank =
    if isBlank then
        red

    else
        black


toElement : ( String, Bool ) -> Element msg
toElement ( part, isBlank ) =
    el [ Element.Font.color <| elementColor isBlank ] <| text part


toGroups : String -> String -> List ( String, Bool )
toGroups left right =
    let
        pairs =
            List.map2 Tuple.pair (String.toList left) (String.toList right)
    in
    List.foldl combine [] pairs |> List.reverse


combine : ( Char, Char ) -> List ( String, Bool ) -> List ( String, Bool )
combine ( l, r ) list =
    if l == r then
        case List.head list of
            Nothing ->
                [ ( String.fromChar l, False ) ]

            Just ( str, b ) ->
                if not b then
                    ( str ++ String.fromChar l, False ) :: List.drop 1 list

                else
                    ( String.fromChar l, False ) :: list

    else
        case List.head list of
            Nothing ->
                [ ( String.fromChar l, True ) ]

            Just ( str, b ) ->
                if b then
                    ( str ++ String.fromChar l, True ) :: List.drop 1 list

                else
                    ( String.fromChar l, True ) :: list
