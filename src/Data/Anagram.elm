module Data.Anagram exposing (AnagramResponse, fetchResponse)

import Array
import Browser
import Element exposing (..)
import Element.Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JD
import List.Extra as List
import Task


type alias AnagramResponse =
    { fullWord : String, withBlanks : String }


fetchResponse : String -> String -> (Result Http.Error (List AnagramResponse) -> msg) -> Cmd msg
fetchResponse query pattern msg =
    Http.get ("http://mbaumann.info:3000/search?letters=" ++ query ++ "&pattern=" ++ pattern) decodeAnagramList
        |> Http.send msg


decodeAnagramList : JD.Decoder (List AnagramResponse)
decodeAnagramList =
    JD.list decodeAnagram


decodeAnagram : JD.Decoder AnagramResponse
decodeAnagram =
    JD.succeed AnagramResponse
        |> JD.required "a" JD.string
        |> JD.required "b" JD.string
