module Data.Language exposing (Language, init)

import Char exposing (fromCode)
import Data.Tile exposing (TileDef, blanko, cleanInput)
import Dict exposing (Dict)
import String exposing (fromChar)


type alias Language =
    { name : String
    , tx : String -> String
    , tileDefs : List TileDef
    , cleanInput : String -> String
    }


lookup : Dict String String -> String -> String
lookup dict key =
    Dict.get key dict
        |> Maybe.withDefault ("[**" ++ key ++ "**]")


translations : List ( String, String )
translations =
    [ ( "input-bench-tiles", "Buchstaben auf der Bank:" )
    ]


init : Language
init =
    { name = "Deutsch"
    , tx = lookup <| Dict.fromList translations
    , tileDefs = german
    , cleanInput = Data.Tile.cleanInput german
    }


german : List TileDef
german =
    [ TileDef blanko 0 2
    , TileDef "E" 1 15
    , TileDef "N" 1 9
    , TileDef "S" 1 7
    , TileDef "I" 1 6
    , TileDef "R" 1 6
    , TileDef "T" 1 6
    , TileDef "U" 1 6
    , TileDef "A" 1 5
    , TileDef "D" 1 4
    , TileDef "H" 2 4
    , TileDef "G" 2 3
    , TileDef "L" 2 3
    , TileDef "O" 2 3
    , TileDef "M" 3 4
    , TileDef "B" 3 2
    , TileDef "W" 3 1
    , TileDef "Z" 3 1
    , TileDef "C" 4 2
    , TileDef "F" 4 2
    , TileDef "K" 4 2
    , TileDef "P" 4 1
    , TileDef ae 6 1
    , TileDef "J" 6 1
    , TileDef ue 6 1
    , TileDef "V" 6 1
    , TileDef oe 8 1
    , TileDef "X" 8 1
    , TileDef "Q" 10 1
    , TileDef "Y" 10 1
    ]


ae : String
ae =
    "Ä"


oe : String
oe =
    "Ö"


ue : String
ue =
    "Ü"


umlaut : Int -> String
umlaut code =
    fromChar <| fromCode code
