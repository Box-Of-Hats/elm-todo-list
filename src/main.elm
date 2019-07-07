module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOver)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { tasks : List String
    , completedTasks : List String
    , newTask : String
    }


init : Model
init =
    { tasks = [ "Dont stress", "Make stuff" ]
    , completedTasks = []
    , newTask = ""
    }



-- UPDATE


type Msg
    = AddTask
    | CompleteTask String
    | UpdateTaskBox String
    | UnCompleteTask String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            { model | tasks = model.tasks ++ [ model.newTask ], newTask = "" }

        CompleteTask task ->
            { model
                | tasks = List.filter (areStringsNotEqual task) model.tasks
                , completedTasks = [ task ] ++ model.completedTasks
            }

        UpdateTaskBox text ->
            { model | newTask = text }

        UnCompleteTask task ->
            { model
                | completedTasks = List.filter (areStringsNotEqual task) model.completedTasks
                , tasks = model.tasks ++ [ task ]
            }


areStringsNotEqual : String -> String -> Bool
areStringsNotEqual a b =
    not (a == b)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "tasks-app" ]
        ([ renderTextInput model.newTask ]
            ++ List.map renderTask model.tasks
            ++ List.map renderCompletedTask model.completedTasks
        )



-- Helper to render a single task


renderTask : String -> Html Msg
renderTask task =
    div [ class "tasks-app__task" ] [ text task, div [ class "tasks-app__check", onClick (CompleteTask task) ] [] ]



-- Render the task input


renderTextInput : String -> Html Msg
renderTextInput textFieldValue =
    div [ class "tasks-app__form" ]
        [ input [ class "tasks-app__input", onInput UpdateTaskBox, placeholder "New task", value textFieldValue ] []
        , input [ class "tasks-app__add", onClick AddTask, type_ "button", value "+" ] []
        ]



-- Render completed tasks


renderCompletedTask : String -> Html Msg
renderCompletedTask task =
    div [ class "tasks-app__task tasks-app__task--completed" ] [ text task, div [ class "tasks-app__check tasks-app__check--completed", onClick (UnCompleteTask task) ] [] ]
