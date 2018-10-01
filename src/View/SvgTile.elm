module View.SvgTile exposing (arrange, viewArranged)

import Data.Tile exposing (Tile)
import Html exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, add, getX, getY, scale, vec2)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (..)
import View.Style as Style exposing (..)


viewArranged : Vec2 -> Float -> Float -> List Tile -> Svg msg
viewArranged pos width spacing list =
    let
        ( tx, ty ) =
            toStringTuple pos

        positions =
            List.foldl (arrange width spacing) [] list

        tiles =
            List.map2 (\p t -> { t | pos = p }) positions list
    in
    List.map view tiles |> Svg.g [ SA.transform <| "translate (" ++ tx ++ "," ++ ty ++ ")" ]


arrange : Float -> Float -> Tile -> List Vec2 -> List Vec2
arrange max spacing tile list =
    case List.head <| List.reverse list of
        Nothing ->
            [ vec2 0 0 ]

        Just pos ->
            let
                x_ =
                    getX pos + getX tile.size + spacing

                nextPos =
                    if x_ > max then
                        ( 0, getY pos + getY tile.size + spacing )

                    else
                        ( x_, getY pos )
            in
            list ++ [ vec2 (Tuple.first nextPos) (Tuple.second nextPos) ]


relPos : Float -> Float -> Float
relPos size factor =
    (-size / 2) + factor * (size / 19)


svgTile : Tile -> Svg msg
svgTile model =
    let
        pos =
            vec2 (getX model.size / -2) (getY model.size / -2)

        radius =
            getX model.size / 10
    in
    roundedRect pos model.size radius <| fromList (tileStyle model)


svgLetter : Tile -> Svg msg
svgLetter model =
    let
        pos =
            vec2 (relPos (getX model.size) 8) (relPos (getY model.size) 8)

        fontHeight =
            getY model.size * 0.7
    in
    makeText model.def.letter pos fontHeight <| fromList letterStyle


svgValue : Tile -> Svg msg
svgValue model =
    let
        pos =
            vec2 (relPos (getX model.size) 15) (relPos (getY model.size) 13.5)

        fontHeight =
            getY model.size * 0.3
    in
    makeText (String.fromInt model.def.value) pos fontHeight <| fromList valueStyle


svgId : Tile -> Svg msg
svgId model =
    let
        pos =
            vec2 (relPos (getX model.size) 15) (relPos (getY model.size) 1)

        fontHeight =
            getY model.size * 0.3
    in
    makeText (String.fromInt model.id) pos fontHeight <| fromList idStyle


toSvg : Tile -> Svg msg
toSvg model =
    let
        ( px, py ) =
            toStringTuple model.pos

        tx =
            "translate (" ++ px ++ "," ++ py ++ ")"

        transformString =
            tx

        --"rotate(" ++ (toString model.id) ++ ") " ++ tx
        groupStyle =
            fromList cursorHand
    in
    g
        [ transform transformString
        , SA.style groupStyle

        -- , SE.onMouseOver (Material.Tile.Hover model.id True)
        -- , SE.onMouseOut (Material.Tile.Hover model.id False)
        ]
        [ svgTile model
        , svgLetter model
        , svgValue model

        --, svgId model
        --, Svg.circle [x "0", y "0", r "1", fill "red"]  []
        ]


view : Tile -> Svg msg
view model =
    toSvg model



-------------------- STYLES


tileStyle : Tile -> List ( String, String )
tileStyle model =
    let
        color =
            if model.hovering then
                "green"

            else
                "yellow"
    in
    defaultStroke ++ [ ( "fill", color ) ] ++ cursorHand


letterStyle : List ( String, String )
letterStyle =
    [ ( "font-family", "Arial Black" )
    , ( "fill", "black" )
    , ( "text-anchor", "middle" )
    , ( "alignment-baseline", "central" )
    ]
        ++ unselectable
        ++ cursorHand


valueStyle : List ( String, String )
valueStyle =
    [ ( "font-family", "Arial" )
    , ( "font-weight", "bold" )
    , ( "fill", "black" )
    , ( "text-anchor", "middle" )
    , ( "alignment-baseline", "hanging" )
    , ( "letter-spacing", "-0.1em" )
    ]
        ++ unselectable
        ++ cursorHand


idStyle : List ( String, String )
idStyle =
    [ ( "font-family", "Arial" )
    , ( "fill", "red" )
    , ( "text-anchor", "middle" )
    , ( "alignment-baseline", "hanging" )
    , ( "letter-spacing", "-0.1em" )
    ]
        ++ unselectable
        ++ cursorHand



--      angle = m.angle + toOffset m.animationState
--        g [ transform (txString ++ " rotate(" ++ toString angle ++ ") scale("++ toString scale ++ " " ++ toString scale ++ ")")
--          , onClick (Signal.message address (Move (500,100)))
--          , onMouseOver (Signal.message address (Hover True))
--          , onMouseOut (Signal.message address (Hover False))
--          ]


toStringTuple : Vec2 -> ( String, String )
toStringTuple v =
    ( String.fromFloat (getX v)
    , String.fromFloat (getY v)
    )


roundedRect : Vec2 -> Vec2 -> Float -> String -> Svg a
roundedRect pos size cornerRadius styleString =
    let
        ( px, py ) =
            toStringTuple pos

        ( sx, sy ) =
            toStringTuple size

        r =
            String.fromFloat cornerRadius
    in
    Svg.rect
        [ x px
        , y py
        , SA.width sx
        , SA.height sy
        , rx r
        , ry r
        , SA.style styleString
        ]
        []


makeText : String -> Vec2 -> Float -> String -> Svg a
makeText str pos textHeight textStyle =
    text_
        [ SA.x (String.fromFloat <| getX pos)
        , SA.y (String.fromFloat <| getY pos)
        , SA.fontSize (String.fromFloat textHeight)
        , SA.style textStyle
        ]
        [ Svg.text str ]
