module Tasklist exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Tasks exposing (Task, TaskId, tasksDecoder)
import RemoteData exposing (WebData)


type alias Model =
    {tasks : WebData (List Task),
     filter : String,
     closed : Bool,
     sort : Int  
    }


type Msg
    = GetTasks
    | GotTasks (WebData (List Task))
    | UpdateFilter String
    | UpdateSort String
    | Closed Bool


init : ( Model, Cmd Msg )
init =
    ( initialModel, getTasks )

initialModel : Model
initialModel = 
    {tasks = RemoteData.Loading,
     filter = "",
     closed = False,
     sort = 0}


getTasks : Cmd Msg
getTasks =
    Http.get
        { url = "http://localhost:5019/tasks/"
        , expect =
            tasksDecoder
                |> Http.expectJson (RemoteData.fromResult >> GotTasks)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTasks ->
            ( { model | tasks = RemoteData.Loading }, getTasks )
        GotTasks response ->
            ( { model | tasks = response }, Cmd.none )
        UpdateFilter newFilter ->
            ( { model | filter = newFilter }, Cmd.none )
        UpdateSort newSort ->
            ( { model | sort = (Maybe.withDefault 0 (String.toInt newSort))}, Cmd.none)
        Closed state ->
            ( { model | closed = state }, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        --[ button [ onClick Fetchtasks ] [ text "Refresh" ]
        [h4 [align "center"][text "TASK MANAGER"]
        , div []
            [ text "Filter"
            , br [] []
            , input [ type_ "text", onInput UpdateFilter ] []
            ]
        , div []
            [ text "Sort"
            , br [] []
            , input [ type_ "number", onInput UpdateSort ] []
            ]
        , viewTasks model.tasks model
        , br[][]
        , br[][]
        , a [href "/tasks/new"] [text "Add Task"]
        ]

descriptionFilter : String -> Task -> Bool 
descriptionFilter s t =
    String.contains s (String.toLower t.description)

viewTasks : WebData (List Task) -> Model -> Html Msg
viewTasks tasks model =
    case tasks of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Tasks..." ]

        RemoteData.Success givenTasks ->
            let
                actualTasks1 = List.filter (descriptionFilter model.filter) givenTasks
                actualTasks = taskSort model.sort actualTasks1 
            in
                div []
                    [ button [type_ "button", onClick (UpdateSort "0") ] [ text "ID" ]
                    , button [type_ "button", onClick (UpdateSort "1") ] [ text "Title" ]
                    , button [type_ "button", onClick (UpdateSort "2") ] [ text "Description" ]
                    , button [type_ "button", onClick (UpdateSort "3") ] [ text "Opened" ]
                    , button [type_ "button", onClick (UpdateSort "4") ] [ text "Duration" ]
                    , h3 [] [ text "Open Tasks" ]
                    , table []
                        ([ viewTableHeader ] ++ List.map viewOpenTask actualTasks)
                    , br [][], br[][]
                    , text "Show closed tasks? "
                    , input [type_ "checkbox", checked model.closed, onClick (Closed (not model.closed))][]
                    , br[][],br[][]
                    , conditionalShow model.closed (h3 [] [ text "Closed Tasks" ])
                    , conditionalShow model.closed (table []
                        ([ viewTableHeader ] ++ List.map viewClosedTask actualTasks))
                    ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


conditionalShow : Bool -> Html Msg -> Html Msg
conditionalShow b m =
    case b of
        True -> m
        False -> text ""

taskSort : Int -> List Task -> List Task
taskSort i l =
    case i of
        1 -> List.sortBy (String.toLower << .title) l
        2 -> List.sortBy (String.toLower << .description) l
        3 -> List.sortBy .started l
        4 -> List.sortBy .due l
        _ -> List.sortBy (Tasks.idToString << .id) l

viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "Title" ]
        , th []
            [ text "Description" ]
        , th []
            [ text "Opened" ]
        , th []
            [ text "Duration" ]
        ]


viewOpenTask : Task -> Html Msg
viewOpenTask task =
    case task.status of
        False ->
            let
                path = "/tasks/" ++ Tasks.idToString task.id
            in
                tr [style "background-color" "green"]
                    [ td []
                        [ text (Tasks.idToString task.id) ]
                    , td []
                        [ text task.title ]
                    , td []
                        [ text task.description ]
                    , td []
                        [ text task.started ]
                    , td []
                        [ text ((String.fromInt task.due)++" days") ]
                    , td [style "background-color" "white"]
                        [a [href path] [text "Edit"]]
                    ]
        True -> text ""

viewClosedTask : Task -> Html Msg
viewClosedTask task =
    case task.status of
        True ->
            let
                path = "/tasks/" ++ Tasks.idToString task.id
            in
                tr [style "background-color" "red"]
                    [ td []
                        [ text (Tasks.idToString task.id) ]
                    , td []
                        [ text task.title ]
                    , td []
                        [ text task.description ]
                    , td []
                        [ text task.started ]
                    , td []
                        [ text ((String.fromInt task.due)++" days") ]
                    , td [style "background-color" "white"]
                        [a [href path] [text "Edit"]]
                    ]
        False -> text ""


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch tasks at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message