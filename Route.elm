module Route exposing (Route(..), parseUrl, pushUrl)

import Url exposing (Url)
import Url.Parser exposing (..)
import Tasks exposing (TaskId)
import Browser.Navigation as Nav


type Route
    = NotFound
    | Tasks
    | Task TaskId
    | NewTask


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound

matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Tasks top
        , map Tasks (s "tasks")
        , map Task (s "tasks" </> Tasks.idParser)
        , map NewTask (s "tasks" </> s "new")
        ]

pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey

routeToString : Route -> String
routeToString route =
    case route of
        NotFound -> "/not-found"
        Tasks -> "/tasks"
        Task taskId -> "/tasks/" ++ Tasks.idToString taskId
        NewTask -> "tasks/new"