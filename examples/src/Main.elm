module Main exposing (main)


import Browser exposing (Document)
import Html exposing (Html)
import Process
import Task exposing (Task)
import Task.Extra as Task exposing (Concurrent, Join)


type Model
    = Running (Concurrent String Int Msg)
    | Bad String
    | Good Int


type Msg
    = OnEach (Join String Int)
    | OnEnd (Result (List String) (List Int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Running fibres ->
            case msg of
                OnEach n ->
                    Task.join n fibres
                        |> Tuple.mapFirst Running

                OnEnd (Ok nums) ->
                    ( List.sum nums |> Good, Cmd.none )

                OnEnd (Err msgs) ->
                    ( String.join " " msgs |> Bad, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    case model of
        Running fibres ->
            { title = "Running", body = [ Html.text "Running" ] }

        Bad reason ->
            { title = "Sad", body = [ Html.text reason ] }

        Good result ->
            { title = "Happy", body = [ String.fromInt result |> Html.text ] }


task1 : Task String Int
task1 =
    Process.sleep 1000
        |> Task.andThen (\_ -> Task.succeed 1)


task2 : Task String Int
task2 =
    Task.succeed 2


task3 : Task String Int
task3 =
    Task.fail "Sorry"


task4 : Task String Int
task4 =
    Process.sleep 1100
        |> Task.andThen (\_ -> Task.fail "i am late :(")


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                Task.concurrent OnEach OnEnd [ task3, task2, task1, task4 ]
                    |> Tuple.mapFirst Running
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
