module GraphSearch exposing (dfs, SearchResult, Neighbourhood)

{-| Minimal set of graph search algorithms, designed to be as representation
agnostic as possible. Leads to some functions with a large number of arguments.


# Functions

@docs dfs

-}

import List
import Set exposing (Set)


type alias Neighbourhood a =
    a -> List a


type alias SearchResult a =
    ( Set a, List a )


{-| Basic depth-first search. Requires three arguments: a set of elements already
visited, a function to take an element and generate its neighbours and a
starting position. The result is the updated set of visted elements and a
list of all the nodes visited (pre-order).

It's up to the caller to supply the neighbourhood function, this implicitly
defines the whole graph -- how it is done can have a strong effect on the
algorithmic complexity of the whole thing so be a little bit careful.

-}
dfs : Set comparable -> Neighbourhood comparable -> comparable -> SearchResult comparable
dfs visited neighbours start =
    List.foldl (dfsFold neighbours) ( visited, [] ) <|
        (List.filter (not << flip Set.member visited) <| neighbours start)


dfsFold : Neighbourhood comparable -> (comparable -> SearchResult comparable -> SearchResult comparable)
dfsFold neighbours =
    let
        foldInner val ( visited, path ) =
            case Set.member val visited of
                True ->
                    ( visited, path )

                False ->
                    let
                        ( newvis, newpath ) =
                            dfs (Set.insert val visited) neighbours val
                    in
                        ( newvis, path ++ [ val ] ++ newpath )
    in
        foldInner
