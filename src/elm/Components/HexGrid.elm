module Components.HexGrid exposing (..)

import Html exposing (..)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
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


svgHexagon : number -> number -> String -> Svg msg
svgHexagon x y f =
    polygon
        [ points << String.join " " << List.map formatPoint <| hexagonPoints
        , transform <| "translate(" ++ toString x ++ "," ++ toString y ++ "),scale(1)"
        , fill f
        ]
        []


hexagonGrid : Html msg
hexagonGrid =
    svg
        []
        [ svgHexagon 0 0 "#cdaaaa" ]
