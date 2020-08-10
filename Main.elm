module Main exposing (main)

import Browser exposing (UrlRequest, Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Tasklist
import EditTask
import NewTask
import Route exposing (Route)
import Html exposing (..)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }

type Page
    = NotFoundPage
    | ListPage Tasklist.Model
    | EditPage EditTask.Model
    | NewPage NewTask.Model

type Msg
    = ListPageMsg Tasklist.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url
    | EditPageMsg EditTask.Msg
    | NewPageMsg NewTask.Msg

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )

initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Tasks ->
                    let
                        ( pageModel, pageCmds ) =
                            Tasklist.init
                    in
                    ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )
                Route.Task taskid ->
                    let
                        (pageModel, pageCmd) = EditTask.init taskid model.navKey
                    in
                        (EditPage pageModel, Cmd.map EditPageMsg pageCmd)
                Route.NewTask ->
                    let
                        (pageModel, pageCmd) = NewTask.init model.navKey
                    in
                        (NewPage pageModel, Cmd.map NewPageMsg pageCmd)
    in
        ( { model | page = currentPage }
        , Cmd.batch [ existingCmds, mappedPageCmds ]
        )

view : Model -> Document Msg
view model =
    {title = "Tracker"
    , body = [currentView model]}

currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView
        ListPage pageModel ->
            Tasklist.view pageModel
                |> Html.map ListPageMsg
        EditPage pageModel ->
            EditTask.view pageModel
                |> Html.map EditPageMsg
        NewPage pageModel ->
            NewTask.view pageModel
                |> Html.map NewPageMsg

notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Tasklist.update subMsg pageModel
            in
            ( { model | page = ListPage updatedPageModel }
            , Cmd.map ListPageMsg updatedCmd
            )
        ( LinkClicked (Browser.Internal url), _) ->
            (model, Nav.pushUrl model.navKey (Url.toString url))
        ( UrlChanged url, _) ->
            let
                newRoute = Route.parseUrl url
            in
                ({model | route = newRoute}, Cmd.none) |> initCurrentPage 
        ( EditPageMsg subMsg, EditPage pageModel) -> 
            let
                (updatedPageModel, updatedCmd) = EditTask.update subMsg pageModel
            in
                ({model | page = EditPage updatedPageModel}, Cmd.map EditPageMsg updatedCmd)
        ( NewPageMsg subMsg, NewPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    NewTask.update subMsg pageModel
            in
            ( { model | page = NewPage updatedPageModel }
            , Cmd.map NewPageMsg updatedCmd
            )
        ( _, _ ) ->
            ( model, Cmd.none )