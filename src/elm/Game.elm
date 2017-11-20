module Game exposing (Agent, Board, GameState, Player, Position)

{-| This module contains the datatypes and logic necessary for actually
playing the game.


# Key datatypes

@docs GameState, Board, Agent, Player, Position


# Important functions

???

-}

import Array exposing (Array, get, set)
import Random
import Maybe.Extra exposing (join)


{-| A supported player. This is used to know how to fetch an action every time
a move needs to be made. For example, if it is a human player we will need to
just wait until they click on something or whatever the interaction mechanism
is. Conversely, if it is a computer player (eg. Random) we will have to trigger
a function call or a random number generation etc.
Currently the possible players are:

  - Human (which triggers no commands)
  - Random (which chooses totally at random)

-}
type Agent
    = Random (Random.Generator Int)
    | Human


{-| Enumeration of the possible players. There are two.
-}
type Player
    = Player1
    | Player2


{-| The board. This is a grid of cells, each of which will take either the
value of the player who has placed a stone there or nothing. They are
therefore represented with Maybe type. The grid has to be a square so we only
specify it's size by one number.
-}
type alias Board a =
    { cells : Array (Maybe a), size : Int }


{-| Just a grid position. We are viewing our hex grid as a tilted square, so we
are using 2D coordinates.
-}
type alias Position =
    { x : Int, y : Int }


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


{-| Flatten a Position into an array index. Needs to also know the width
-}
flattenPosition : Int -> Position -> Int
flattenPosition size pos =
    pos.x + pos.y * size


{-| Get a position on the board. A maybe because it might not be a valid
position
-}
boardGetAt : Board a -> Position -> Maybe a
boardGetAt board =
    join << flip get board.cells << flattenPosition board.size


inRange : Int -> Int -> Int -> Bool
inRange max_ min_ num =
    (num >= min_) && (num < max_)


{-| Check if a position is actually on a board
-}
positionOnBoard : Board a -> Position -> Bool
positionOnBoard board pos =
    if inRange board.size 0 pos.x && inRange board.size 0 pos.y then
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
                            set (flattenPosition board.size position)
                                (Just player)
                                board.cells
                    }
            else
                Nothing
