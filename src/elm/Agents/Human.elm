module Agents.Human exposing (view, HumanState, nextMove)

{-|


# A human agent

View and state business for handling human players.


# functions

@docs view, nextMove


# types

@docs HumanState

-}

import Html exposing (..)
import Game exposing (Player)


{-| The human agent only needs to keep track of which player it is so that it
can draw things in the right colour.
-}
type alias HumanState =
    { player : Player }



{- View stuff. For the human player we will do a little box asking for a
   move and probably not much else.
-}


{-| The stuff we see
-}
view : HumanState -> Html msg
view state =
    div [] []



{- "Logic" to choose a new move. In this case it just means waiting for a key
   input and translating it.
-}


{-| Get a move, aka do nothing until a mouse input
-}
nextMove : HumanState -> Cmd msg
nextMove humanState =
    Cmd.none
