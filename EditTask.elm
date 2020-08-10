module EditTask exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Tasks exposing (Task, TaskId, taskDecoder, taskEncoder)
import RemoteData exposing (WebData)
import Browser.Navigation as Nav
import Route
import String

type alias Model 
    = {navKey : Nav.Key,
       task : WebData Task,
       saveError : Maybe String}

type Msg
    = TaskReceived (WebData Task)
    | UpdateTitle String
    | UpdateDescription String
    | UpdateStatus Bool
    | UpdateDuration Int
    | SaveTask
    | TaskSaved (Result Http.Error Task)

init : TaskId -> Nav.Key -> ( Model, Cmd Msg )
init taskId navKey = (initialModel navKey, fetchTask taskId)


initialModel : Nav.Key -> Model
initialModel navKey 
    = {navKey = navKey,
       task = RemoteData.Loading,
       saveError = Nothing}

fetchTask : TaskId -> Cmd Msg
fetchTask taskId =
    Http.get
        { url = "http://localhost:5019/tasks/" ++ Tasks.idToString taskId
        , expect =
            taskDecoder
                |> Http.expectJson (RemoteData.fromResult >> TaskReceived)
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TaskReceived task ->
            ( { model | task = task }, Cmd.none )
        UpdateTitle newTitle ->
            let
                updateTitle = 
                    RemoteData.map
                        (\taskData ->
                            { taskData | title = newTitle }
                        )
                        model.task
            in
                ({model | task = updateTitle}, Cmd.none)
        UpdateDescription newDescription ->
            let
                updateDescription = 
                    RemoteData.map
                        (\taskData ->
                            { taskData | description = newDescription }
                        )
                        model.task
            in
                ({model | task = updateDescription}, Cmd.none)
        UpdateStatus newStatus ->
            let
                updateStatus = 
                    RemoteData.map
                        (\taskData ->
                            { taskData | status = newStatus }
                        )
                        model.task
            in
                ({model | task = updateStatus}, Cmd.none)
        UpdateDuration newDuration ->
            let
                updateDuration = 
                    RemoteData.map
                        (\taskData ->
                            { taskData | due = newDuration }
                        )
                        model.task
            in
                ({model | task = updateDuration}, Cmd.none)
        SaveTask -> (model, saveTask model.task)
        TaskSaved (Ok taskData) ->
            let
                task = RemoteData.succeed taskData
            in
                ({model | task = task, saveError = Nothing}, 
                Route.pushUrl Route.Tasks model.navKey)
        TaskSaved (Err error) -> ({model | saveError = Just (buildErrorMessage error)}, Cmd.none)

saveTask : WebData Task -> Cmd Msg
saveTask task =
    case task of
        RemoteData.Success taskData ->
            let
                taskUrl =
                    "http://localhost:5019/tasks/"
                        ++ Tasks.idToString taskData.id
            in
            Http.request
                { method = "PATCH"
                , headers = []
                , url = taskUrl
                , body = Http.jsonBody (taskEncoder taskData)
                , expect = Http.expectJson TaskSaved taskDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none

view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit Task" ]
        , viewTask model.task
        , viewSaveError model.saveError
        ]


viewTask : WebData Task -> Html Msg
viewTask task =
    case task of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Task..." ]

        RemoteData.Success taskData ->
            editForm taskData

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


editForm : Task -> Html Msg
editForm task =
    Html.form []
        [ div []
            [ text "Title"
            , br [] []
            , input
                [ type_ "text"
                , value task.title
                , onInput UpdateTitle
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Description"
            , br [] []
            , input
                [ type_ "text"
                , value task.description
                , onInput UpdateDescription
                ]
                []
            ]
        , br [] []
        , text ("Current status: " ++ (boolString task.status))
        , input [type_ "checkbox", checked task.status, onClick (UpdateStatus (not task.status))][]
        , br [] []
        , div []
            [ text "Days Left: "
            , br [] []
            , input
                [ type_ "number"
                , value (String.fromInt task.due)
                , onInput (UpdateDuration << intExtract)
                ]
                []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick SaveTask ]
                [ text "Submit" ]
            ]
        ]

intExtract : String -> Int
intExtract s =
    case String.toInt s of
        Nothing -> 0
        Just i -> i

boolString : Bool -> String
boolString b =
    case b of 
        True -> "Completed"
        False -> "Uncompleted"

viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch task at this time."
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

viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't save task at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""