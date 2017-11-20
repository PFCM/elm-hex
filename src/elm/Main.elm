module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- component import example

import Components.HexGrid exposing (hexagonGrid)
import Game


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { gameState : Game.GameState }


model : Model
model =
    { gameState = Game.newGame Game.HumanAgent Game.HumanAgent 11 }



-- UPDATE


type Msg
    = NoOp
    | GridClick Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        GridClick flatIndex ->
            { model | gameState = Game.move model.gameState flatIndex }



-- VIEW


getHexClass : Model -> Int -> String
getHexClass model flatIndex =
    let
        val =
            Game.boardGetAt model.gameState.board flatIndex
    in
        case val of
            Just player ->
                case player of
                    Game.Player1 ->
                        "hex-filled-player1"

                    Game.Player2 ->
                        "hex-filled-player2"

            Nothing ->
                "hex-empty"


playerName : Game.Player -> String
playerName player =
    case player of
        Game.Player1 ->
            "Player 1"

        Game.Player2 ->
            "Player 2"


playerClass : Game.Player -> String
playerClass player =
    case player of
        Game.Player1 ->
            "player1"

        Game.Player2 ->
            "player2"


turnBox : Model -> Html msg
turnBox model =
    div [ class "turn-box" ]
        [ div [ class << playerClass <| model.gameState.nextTurn ]
            [ text ((playerName model.gameState.nextTurn) ++ "'s ")
            ]
        , text "turn."
        ]


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
        [ turnBox model
        , hexagonGrid 5 GridClick (getHexClass model)
        ]



-- CSS STYLES


styles : { img : List ( String, String ) }
styles =
    { img =
        [ ( "width", "33%" )
        , ( "border", "4px solid #337AB7" )
        ]
    }
