module Task.Extra exposing
    ( andMap
    , toCmd
    , Concurrent(..), Join
    , concurrent, join
    )

{-|


# Applicative

@docs andMap


# Utils

@docs toCmd


# Concurrent


# definitions

@docs Concurrent, Join


# Concurrent utilities

@docs concurrent, join

-}

import Task exposing (Task)


{-| Applicative task
-}
andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    Task.map2 (|>)


{-| Convert message to Cmd
-}
toCmd : msg -> Cmd msg
toCmd =
    Task.succeed >> Task.perform identity


{-| Opaque type to hold state of running tasks
-}
type Concurrent e a msg
    = Concurrent (Result (List e) (List a) -> msg) (List (Maybe (Result e a)))


{-| Opaque type to hold idividual result for joining
-}
type Join e a
    = Join Int (Result e a)


{-| Start execution of concurrent tasks and return inital state
-}
concurrent : (Join e a -> msg) -> (Result (List e) (List a) -> msg) -> List (Task e a) -> ( Concurrent e a msg, Cmd msg )
concurrent onEach onEnd tasks =
    ( Concurrent onEnd (List.repeat (List.length tasks) Nothing)
    , if List.isEmpty tasks then
        Ok []
            |> onEnd
            |> toCmd

      else
        List.indexedMap (\ix task -> Task.attempt (Join ix >> onEach) task) tasks |> Cmd.batch
    )


{-| Accumulate returned results and produce Cmd
-}
join : Join e a -> Concurrent e a msg -> ( Concurrent e a msg, Cmd msg )
join (Join ix result) (Concurrent onEnd tasks) =
    let
        joined =
            List.indexedMap
                (\i f ->
                    if i == ix then
                        Just result

                    else
                        f
                )
                tasks
    in
    ( Concurrent onEnd joined
    , if List.any ((==) Nothing) joined then
        Cmd.none

      else
        List.filterMap identity joined
            |> List.foldl combineResults (Ok [])
            |> onEnd
            |> toCmd
    )


{-| Accumulate results
-}
combineResults : Result e a -> Result (List e) (List a) -> Result (List e) (List a)
combineResults res acc =
    case ( res, acc ) of
        ( Err e, Err es ) ->
            Err (es ++ [ e ])

        ( Err e, Ok _ ) ->
            Err [ e ]

        ( Ok _, Err es ) ->
            Err es

        ( Ok x, Ok xs ) ->
            Ok (xs ++ [ x ])
