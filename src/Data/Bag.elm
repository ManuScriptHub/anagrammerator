module Data.Bag exposing (Bag, fromLanguage, init, pickTile)

-- import Extra.Vec2 exposing (origin)
--, (<+>), fromTuple, toTuple)

import Data.Language exposing (Language)
import Data.Tile as Tile exposing (Tile, TileDef, TileList)
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (foldl, length, map)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)


type alias Bag =
    TileList


init : Bag
init =
    []


pickTile : String -> Bag -> Bag
pickTile letter model =
    case Tile.findTile letter model of
        Nothing ->
            model

        Just tile ->
            Tile.removeTile tile model


fillBag : TileDef -> ( Int, Bag ) -> ( Int, Bag )
fillBag tileDef ( startId, oldBag ) =
    let
        newTiles =
            map (Tile.init tileDef (vec2 Tile.defaultSize Tile.defaultSize)) (List.range startId (startId + tileDef.count - 1))

        nextStartId =
            startId + length newTiles
    in
    ( nextStartId, oldBag ++ newTiles )


fromList : List TileDef -> Bag
fromList tileDefs =
    Tuple.second (foldl fillBag ( 0, init ) tileDefs)


fromLanguage : Language -> Bag
fromLanguage language =
    fromList language.tileDefs



-- toForm: Bag->Form
-- toForm model =
--     let sorted = List.sortBy .id model.tiles
--         positions = List.foldl (arrange 400 3) [] sorted
--         forms = map Material.Tile.toForm sorted
--         arranged = List.map2 (\pos form -> Co.move (toTuple pos) form) positions forms
--     in group [group arranged, filled Color.red (circle 3)]
--let sorted = List.sortBy .id model.tiles
--    positions = List.foldl (arrange 400 3) [] sorted
--in { model | tiles = List.map2 (\t p -> Material.Tile.update (SetPos p) t) sorted positions }
--Util.Svg.fromList (Vec2 (17*25) 300) <| map Material.Tile.toSvg <| List.sortBy .id model.tiles
