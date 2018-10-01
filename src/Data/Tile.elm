module Data.Tile exposing
    ( Tile
    , TileDef
    , TileId
    , TileList
    , addTile
    , blanko
    , cleanInput
    , defaultSize
    , findTile
    , init
    , removeTile
    , tilesToString
    )

-- import Regex

import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import String


type alias TileId =
    Int


type alias TileDef =
    { letter : String, value : Int, count : Int }


type alias Tile =
    { id : TileId
    , def : TileDef
    , blankoLetter : Maybe String
    , hovering : Bool
    , pos : Vec2
    , size : Vec2
    }


type alias TileList =
    List Tile


defaultSize : number
defaultSize =
    35


init : TileDef -> Vec2 -> TileId -> Tile
init def size id =
    { id = id, def = def, size = size, pos = vec2 0 0, hovering = False, blankoLetter = Nothing }


blanko : String
blanko =
    " "


tilesToString : List Tile -> String
tilesToString tiles =
    List.map (.letter << .def) tiles
        |> String.join ""
        |> String.replace " " "_"


containsTile : Tile -> List Tile -> Bool
containsTile tile list =
    List.any ((==) tile.id << .id) list


removeTile : Tile -> List Tile -> List Tile
removeTile tile list =
    List.filter ((/=) tile.id << .id) list


addTile : Tile -> List Tile -> List Tile
addTile tile list =
    list ++ [ tile ]


findTile : String -> List Tile -> Maybe Tile
findTile str tiles =
    let
        final =
            str
                |> String.replace "-" " "
                |> String.replace "_" " "
    in
    List.head <| List.filter ((==) final << .letter << .def) tiles



{-
   CAUTION: this will not work for non-single-letter tiledefs
   ALSO CAUTION: toUpper is being used, which needs testing for .e.g turkish

-}
--


replaceBlanks : String -> String -> String
replaceBlanks replacement input =
    input
        |> String.replace " " replacement
        |> String.replace "-" replacement
        |> String.replace "_" replacement


containsChar : List String -> Char -> Bool
containsChar str c =
    List.member (String.toUpper <| String.fromChar c) str


cleanInput : List TileDef -> String -> String
cleanInput defs str =
    let
        isValid =
            containsChar <|
                List.map .letter defs
    in
    replaceBlanks blanko str
        |> String.toUpper
        |> String.toList
        |> List.filter (\c -> isValid (Debug.log "c" c))
        |> String.fromList
        |> replaceBlanks "?"
