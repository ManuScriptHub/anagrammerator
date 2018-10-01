module Data.Move exposing (Move, addTile, backspace, cursorLeft, cursorRight, init)

import Array exposing (Array, toIndexedList)
import Data.Tile as Tile exposing (Tile, TileList)
import Json.Decode as JD
import Json.Decode.Pipeline as JD
import List.Extra


type alias Move =
    { tiles : TileList, cursorPos : Int }


init =
    { tiles = [], cursorPos = 0 }


addTile : Tile -> Move -> Move
addTile tile model =
    { model
        | tiles = Tile.addTile tile model.tiles
        , cursorPos = model.cursorPos + 1
    }


backspace : Move -> Move
backspace model =
    let
        newTiles =
            List.Extra.removeIfIndex ((==) (model.cursorPos - 1)) model.tiles
    in
    { model
        | tiles = newTiles
        , cursorPos = model.cursorPos - 1
    }


cursorLeft : Move -> Move
cursorLeft model =
    case model.tiles of
        [] ->
            model

        _ ->
            { model | cursorPos = model.cursorPos - 1 }


cursorRight : Move -> Move
cursorRight model =
    if model.cursorPos >= List.length model.tiles then
        model

    else
        { model | cursorPos = model.cursorPos + 1 }
