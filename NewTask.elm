module NewTask exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Tasks exposing (Task, TaskId, emptyTask, taskDecoder, newTaskEncoder)
import Http
import Route
import Time
import String
import Calendar
import Task

type Msg
    = StoreTitle String
    | StoreDescription String
    | StoreDuration Int
    | PullTime
    | CreateTask
    | TaskCreated (Result Http.Error Task)
    | Zone Time.Zone
    | Now Time.Posix

init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, Task.perform Zone Time.here )

type alias Model =
    {navKey : Nav.Key,
     task : Task,
     createError : Maybe String,
     zone : Time.Zone,
     now : Time.Posix
    }

initialModel : Nav.Key -> Model
initialModel navKey =
    {navKey = navKey,
     task = emptyTask,
     createError = Nothing,
     zone = Time.utc,
     now = (Time.millisToPosix 0)
    }

view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Create New Task" ]
        , newTaskForm
        , viewError model.createError
        ]

newTaskForm : Html Msg
newTaskForm =
    Html.form []
        [ div []
            [ text "Title"
            , br [] []
            , input [ type_ "text", onInput StoreTitle ] []
            ]
        , br [] []
        , div []
            [ text "Description"
            , br [] []
            , input [ type_ "text", onInput StoreDescription ] []
            ]
        , br [] []
        , div []
            [ text "Duration"
            , br [] []
            , input [ type_ "number", onInput (StoreDuration << intExtract) ] []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick PullTime ]
                [ text "Set Time" ]
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick CreateTask ]
                [ text "Submit" ]
            ]
        ]

intExtract : String -> Int
intExtract s =
    case String.toInt s of
        Nothing -> 0
        Just i -> i

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Zone zone ->
            ( {model | zone = zone }, Task.perform Now Time.now)
        Now now ->
            ( {model | now = now}, Cmd.none)
        StoreTitle title ->
            let
                oldTask =
                    model.task

                updateTitle =
                    { oldTask | title = title }
            in
            ( { model | task = updateTitle }, Cmd.none )
        StoreDescription name ->
            let
                oldTask =
                    model.task

                updateDescription =
                    { oldTask | description = name }
            in
            ( { model | task = updateDescription }, Cmd.none )
        StoreDuration length ->
            let
                oldTask =
                    model.task

                updateDuration =
                    { oldTask | due = length }
            in
            ( { model | task = updateDuration }, Cmd.none )
        PullTime ->
            let
                oldTask =
                    model.task
                pulledTime = getTime model
                updateStart =
                    { oldTask | started = pulledTime }
            in
            ( { model | task = updateStart }, Cmd.none )
        CreateTask -> 
            (model, createTask model.task)
        TaskCreated (Ok task) ->
            ( { model | task = task, createError = Nothing }
            , Route.pushUrl Route.Tasks model.navKey
            )
        TaskCreated (Err error) -> ({model | createError = Just (buildErrorMessage error)}, Cmd.none)

getTime : Model -> String
getTime model =
    let
        posixTime = model.now
        date = Calendar.fromPosix posixTime
        yearString = String.fromInt (Calendar.getYear date)
        dayString = String.fromInt (Calendar.getDay date)
        monthString = String.fromInt (Calendar.monthToInt (Calendar.getMonth date))
        result = (monthString ++ "-" ++ dayString ++ "-" ++ yearString)
    in
        Debug.log "printing: " result


createTask : Task -> Cmd Msg
createTask task =
    Http.post
        {url = "http://localhost:5019/tasks",
         body = Http.jsonBody (newTaskEncoder task),
         expect = Http.expectJson TaskCreated taskDecoder}

viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't create a task at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""

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