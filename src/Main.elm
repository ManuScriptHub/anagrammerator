module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Browser.Events
import Browser.Navigation
import Data.Anagram exposing (AnagramResponse, fetchResponse)
import Data.Bag as Bag exposing (Bag)
import Data.Key exposing (Key(..), keyDecoder, specialKeyDecoder)
import Data.Language exposing (Language)
import Data.Move as Move exposing (Move)
import Data.Tile as Tile exposing (Tile, tilesToString)
import Element exposing (..)
import Element.Border as Border
import Element.Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JD
import List.Extra as List
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task
import Util exposing (httpErrorToString)
import View.Anagram as Anagram
import View.SvgTile exposing (viewArranged)


type alias AnagramResponse =
    { fullWord : String, withBlanks : String }


type alias Model =
    { pattern : String
    , response : List AnagramResponse
    , error : String
    , language : Language
    , move : Move
    , bag : Bag
    }


type Msg
    = PatternChanged String
    | ResponseReceived (Result Http.Error (List AnagramResponse))
    | OnKey Key
    | OnSpecialKey Key


init : ( Model, Cmd Msg )
init =
    let
        language =
            Data.Language.init
    in
    ( { pattern = ""
      , response = []
      , error = ""
      , language = language
      , bag = Bag.fromLanguage language
      , move = Move.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PatternChanged newPattern ->
            moveChanged { model | pattern = newPattern }

        ResponseReceived (Ok list) ->
            ( { model | response = list }, Cmd.none )

        ResponseReceived (Err err) ->
            ( { model | error = httpErrorToString err }, Cmd.none )

        OnKey key ->
            case key of
                Character char ->
                    if List.length model.move.tiles >= 9 then
                        ( model, Cmd.none )

                    else
                        case Tile.findTile (String.fromChar char |> String.toUpper) model.bag of
                            Nothing ->
                                ( model, Cmd.none )

                            Just tile ->
                                moveChanged
                                    { model
                                        | bag = Tile.removeTile tile model.bag
                                        , move = Move.addTile tile model.move
                                    }

                Control string ->
                    ( model, Cmd.none )

        OnSpecialKey key ->
            case key of
                Character char ->
                    ( model, Cmd.none )

                Control string ->
                    case string of
                        "Backspace" ->
                            case List.getAt (model.move.cursorPos - 1) model.move.tiles of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just tile ->
                                    moveChanged
                                        { model
                                            | move = Move.backspace model.move
                                            , bag = Tile.addTile tile model.bag |> List.sortBy .id
                                        }

                        "ArrowLeft" ->
                            ( { model | move = Move.cursorLeft model.move }, Cmd.none )

                        "ArrowRight" ->
                            ( { model | move = Move.cursorRight model.move }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [ padding 20, width fill, height fill ] <|
        column [ height fill, width fill ]
            [ el [ height fill, width fill ] <| viewSvg model
            , viewResponse model
            ]


viewSvg model =
    [ viewArranged (vec2 500 40) 400 3 model.bag
    , viewArranged (vec2 100 40) 339 3 model.move.tiles
    , Svg.circle [ SA.cx "0", SA.cy "0", SA.r "1", SA.fill "red" ] []
    ]
        |> Svg.svg [ SA.height "400" ]
        |> Element.html
        |> el [ width fill, height fill ]


viewResponse : Model -> Element Msg
viewResponse model =
    column [] <| List.map Anagram.view model.response


patternInput : Model -> Element Msg
patternInput model =
    Input.text []
        { onChange = PatternChanged
        , text = model.pattern
        , placeholder = Just <| Input.placeholder [] <| text "Suchmuster"
        , label =
            Input.labelAbove [] <| text "Suchmuster"
        }



---- PROGRAM ----


moveChanged : Model -> ( Model, Cmd Msg )
moveChanged model =
    ( model, fetchResponse (Debug.log "query" <| tilesToString model.move.tiles) model.pattern ResponseReceived )


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyPress
            (keyDecoder
                |> JD.andThen (\k -> JD.succeed (OnKey k))
            )
        , Browser.Events.onKeyDown
            (specialKeyDecoder
                |> JD.andThen (\k -> JD.succeed (OnSpecialKey k))
            )
        ]
