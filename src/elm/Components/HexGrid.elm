module Components.HexGrid exposing (..)

import Html exposing (..)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import List
import String
import Tuple exposing (first, second)


hexagonCorner : Int -> ( Float, Float )
hexagonCorner i =
    let
        angle =
            degrees (60 * (toFloat i) + 30)
    in
        ( 10 + 10 * cos angle, 10 + 10 * sin angle )


hexagonPoints : List ( Float, Float )
hexagonPoints =
    List.map hexagonCorner <| List.range 0 5


formatPoint : ( Float, Float ) -> String
formatPoint pair =
    (toString << first <| pair) ++ "," ++ (toString << second <| pair)


svgHexagon : number -> number -> msg -> String -> Svg msg
svgHexagon x y clickVal cls =
    polygon
        [ points << String.join " " << List.map formatPoint <| hexagonPoints
        , transform <| "translate(" ++ toString x ++ "," ++ toString y ++ "),scale(1)"
        , class cls
        , onClick clickVal
        ]
        []


makeHexagon : (Int -> msg) -> Int -> (Int -> String) -> Int -> Html msg
makeHexagon clickFn gridSize clsFn num =
    let
        size =
            10

        width =
            (sqrt 3) * size

        height =
            width * 2

        row =
            num % gridSize

        col =
            num // gridSize

        x =
            toFloat row * (width + 2) + (toFloat col) * width / 2

        y =
            toFloat col * width
    in
        svgHexagon x y (clickFn num) (clsFn num)


hexagonGrid : Int -> (Int -> msg) -> (Int -> String) -> Html msg
hexagonGrid gridSize clickFn clsFn =
    svg
        [ viewBox "0 0 500 500" ]
        (List.map
            (makeHexagon clickFn gridSize clsFn)
         <|
            List.range 0 (gridSize * gridSize - 1)
        )
