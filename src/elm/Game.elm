module Game
    exposing
        ( Agent(..)
        , Board
        , GameState
        , Player(..)
        , Position
        , newGame
        , move
        , boardGetAt
        , winner
        )

{-| This module contains the datatypes and logic necessary for actually
playing the game.


# Key datatypes

@docs GameState, Board, Agent, Player, Position


# Important functions

@docs newGame, move, boardGetAt, winner

-}

import Array exposing (Array, get, set)
import Random
import Maybe.Extra as ME exposing (join, isJust)
import Tuple exposing (first, second)
import Set exposing (Set)
import GraphSearch


{-| A supported player. This is used to know how to fetch an action every time
a move needs to be made. For example, if it is a human player we will need to
just wait until they click on something or whatever the interaction mechanism
is. Conversely, if it is a computer player (eg. Random) we will have to trigger
a function call or a random number generation etc.
Currently the possible players are:

  - HumanAgent (which triggers no commands)
  - RandomAgent (which chooses totally at random)

-}
type Agent
    = RandomAgent (Random.Generator Int)
    | HumanAgent


{-| Enumeration of the possible players. There are two.
-}
type Player
    = Player1
    | Player2


{-| given a player, return the other one
-}
otherPlayer : Player -> Player
otherPlayer player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1


{-| The board. This is a grid of cells, each of which will take either the
value of the player who has placed a stone there or nothing. They are
therefore represented with Maybe type. The grid has to be a square so we only
specify it's size by one number.
-}
type alias Board a =
    { cells : Array (Maybe a), size : Int }


{-| Get a new empty board
-}
emptyBoard : Int -> Board a
emptyBoard gridSize =
    { cells = Array.repeat (gridSize * gridSize) Nothing, size = gridSize }


{-| Just a grid position. We are viewing our hex grid as a tilted square, so we
are using 2D coordinates.
-}
type alias Position =
    Int


{-| Represents the current state of a game. This consists of:

  - the players (and any associated state they may bring with them)
  - the board
  - whose turn is next

-}
type alias GameState =
    { player1 : Agent
    , player2 : Agent
    , board : Board Player
    , nextTurn : Player
    }


{-| Make a new empty game state, in which it is player1's turn.
-}
newGame : Agent -> Agent -> Int -> GameState
newGame p1 p2 gridSize =
    { player1 = p1
    , player2 = p2
    , board = emptyBoard gridSize
    , nextTurn = Player1
    }


{-| Get a position on the board. A maybe because it might not be a valid
position
-}
boardGetAt : Board a -> Position -> Maybe a
boardGetAt board =
    join << flip get board.cells


inRange : Int -> Int -> Position -> Bool
inRange min_ max_ num =
    (num >= min_) && (num < max_)


{-| Check if a position is actually on a board
-}
positionOnBoard : Board a -> Position -> Bool
positionOnBoard board pos =
    if inRange 0 (board.size * board.size) pos then
        True
    else
        False


{-| Places a stone on a board for a given player if the move is valid.
A move is invalid if the position is not empty or it is off the grid in
which case it will return Nothing, otherwise an updated board.
-}
placeStone : a -> Position -> Board a -> Maybe (Board a)
placeStone player position board =
    case boardGetAt board position of
        Just _ ->
            Nothing

        -- it's already full
        Nothing ->
            if positionOnBoard board position then
                Just
                    { board
                        | cells =
                            set position (Just player) board.cells
                    }
            else
                Nothing


{-| Unwrap the position into a row and a column index. Visually this could
be confusing, but note that we just count right to left and down from the
top left corner, treating the grid like a square grid that's been a bit
tipped over.
-}
unwrapPosition : Int -> Position -> ( Int, Int )
unwrapPosition gridSize pos =
    ( pos % gridSize, pos // gridSize )


{-| Wrap a (row, column) index into a Position
-}
wrapPosition : Int -> ( Int, Int ) -> Position
wrapPosition gridSize ( row, col ) =
    row + col * gridSize


{-| Check if a (row, column) index would be inside a square grid
-}
inGrid : Int -> ( Int, Int ) -> Bool
inGrid gridSize ( row, column ) =
    List.all (inRange 0 gridSize) [ row, column ]


{-| Get the neighbours of a position. At most there are 6, because hexagon.
Needs to know the size of the grid so it knows when to wrap.
-}
getNeighbours : Int -> Position -> List Position
getNeighbours gridSize position =
    let
        -- unwrap the position because it's easier
        ( row, col ) =
            unwrapPosition gridSize position
    in
        [ ( -1, 0 ), ( 0, -1 ), ( -1, 1 ), ( 0, 1 ), ( 1, 0 ), ( 1, -1 ) ]
            |> List.map (\a -> ( first a + row, second a + col ))
            |> List.filter (inGrid gridSize)
            |> List.map (wrapPosition gridSize)


positionFilter : Board a -> a -> Position -> Maybe Position
positionFilter board val pos =
    case boardGetAt board pos of
        Just newVal ->
            if newVal == val then
                Just pos
            else
                Nothing

        Nothing ->
            Nothing


componentNeighbourhood : Board a -> Position -> List Position
componentNeighbourhood board pos =
    case boardGetAt board pos of
        Just val ->
            getNeighbours board.size pos
                |> List.filterMap (positionFilter board val)

        Nothing ->
            []


{-| Find a single connected component from a given start position, ignoring
positions already visited. In the event of a start position off the board
it will silently return an empty list.
-}
connectedComponent : Set Position -> Board a -> Position -> ( Set Position, List Position )
connectedComponent visited board =
    GraphSearch.dfs
        visited
        (componentNeighbourhood board)


componentFold : Board Player -> Position -> ( Set Position, List (List Position) ) -> ( Set Position, List (List Position) )
componentFold board pos ( visited, components ) =
    let
        ( newvis, comp ) =
            connectedComponent visited board pos
    in
        case comp of
            [] ->
                ( newvis, components )

            component ->
                ( newvis, component :: components )


splitIndex : Player -> ( Int, Int ) -> Int
splitIndex player =
    case player of
        Player1 ->
            first

        Player2 ->
            second


{-| Check if a list of positions should win. If so return their value, otherwise
Nothing
-}
winningComponent : Board Player -> List Position -> Maybe Player
winningComponent board pos =
    let
        val =
            (Maybe.andThen <| boardGetAt board) <| List.head pos

        winning player =
            List.map (unwrapPosition board.size) pos
                |> List.map (splitIndex player)
                |> List.foldl (\i ( a, b ) -> ( a || i == 0, b || i == (board.size - 1) ))
                    ( False, False )
                |> (\pair -> (first pair) && (second pair))
    in
        ME.filter winning val


{-| Checks if a game is over. If it is returns Just the winner, otherwise
Nothing. The algorithm for this is pretty simple:

  - Find all connected components on the board
  - For each component:
      - see if it has at least one position at each end

-}
winner : GameState -> Maybe Player
winner game =
    List.range 0 (game.board.size * game.board.size)
        |> List.filter (isJust << boardGetAt game.board)
        |> List.foldl (componentFold game.board) ( Set.empty, [] )
        |> second
        |> List.filterMap (winningComponent game.board)
        |> List.head


{-| Make a move on a board. This is the highest level function that we expect
to be called by the main controller when stuff happens.
-}
move : GameState -> Int -> GameState
move state pos =
    { state
        | board = Maybe.withDefault state.board << placeStone state.nextTurn pos <| state.board
        , nextTurn = otherPlayer state.nextTurn
    }
