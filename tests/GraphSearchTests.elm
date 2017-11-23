module GraphSearchTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange, array, list)
import Test exposing (..)
import Set exposing (Set)
import GraphSearch exposing (Neighbourhood, SearchResult)
import Tuple exposing (first, second)


type alias SearchAlgorithm comparable =
    Set comparable -> Neighbourhood comparable -> comparable -> SearchResult comparable


fullyConnectedTest : SearchAlgorithm Int -> () -> Expectation
fullyConnectedTest algol () =
    let
        neighbours int =
            List.range 0 10

        -- fully connected, always the same
    in
        algol Set.empty neighbours 0
            |> Expect.all
                [ Expect.equal (List.range 0 10) << second
                , Expect.true "All elements should be visited"
                    << (==) (Set.fromList <| List.range 0 10)
                    << first
                ]


suite : Test
suite =
    describe "The Graph Search module"
        [ describe "Test Depth First search"
            [ test
                "Check it visits every node in a fully connected graph"
                (fullyConnectedTest GraphSearch.dfs)
            ]
        ]
