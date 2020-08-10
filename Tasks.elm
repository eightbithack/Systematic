module Tasks exposing (Task, TaskId, idToString, taskDecoder, emptyTask, 
    tasksDecoder, taskEncoder, idParser, newTaskEncoder)

import Json.Decode as Decode exposing (Decoder, int, list, string, bool)
import Json.Decode.Pipeline exposing (required)
import Url.Parser exposing (Parser, custom)
import Json.Encode as Encode
import Calendar


type alias Task =
    { id : TaskId
    , title : String
    , description : String
    , status : Bool
    , started : String
    , due : Int
    }

type TaskId = TaskId Int

emptyTask : Task
emptyTask =
    {id = (TaskId -1),
     title = "",
     description = "",
     status = False,
     started = "",
     due = 0}

tasksDecoder : Decoder (List Task)
tasksDecoder =
    list taskDecoder

idDecoder : Decoder TaskId
idDecoder =
    Decode.map TaskId int

taskDecoder : Decoder Task
taskDecoder =
    Decode.succeed Task
        |> required "id" idDecoder
        |> required "title" string
        |> required "description" string
        |> required "status" bool
        |> required "started" string
        |> required "due" int

idToString : TaskId -> String
idToString (TaskId id) =
    String.fromInt id

taskEncoder : Task -> Encode.Value
taskEncoder task =
    Encode.object
        [ ( "id", encodeId task.id )
        , ( "title", Encode.string task.title )
        , ( "description", Encode.string task.description )
        , ( "status", Encode.bool task.status )
        , ( "started", Encode.string task.started)
        , ( "due", Encode.int task.due)
        ]

newTaskEncoder : Task -> Encode.Value
newTaskEncoder task =
    Encode.object
        [ ( "title", Encode.string task.title )
        , ( "description", Encode.string task.description )
        , ( "status", Encode.bool task.status )
        , ( "started", Encode.string task.started)
        , ( "due", Encode.int task.due)
        ]

encodeId : TaskId -> Encode.Value
encodeId (TaskId id) =
    Encode.int id

idParser : Parser (TaskId -> a) a
idParser =
    custom "TASKID" <|
        \taskId ->
            Maybe.map TaskId (String.toInt taskId)