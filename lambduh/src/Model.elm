module Model exposing (Model, initialModel, update)

import Dict exposing (Dict)
import Widget exposing (Widget)
import Parser
import LambdaParsers
import Msg exposing (Msg(..))
import LambdaTypes

type alias Model =
    -- starting point for interaction. the user can enter a string that will
    -- be parsed as a lambda term for a term widget
    { termSource : String
    , widgets : Dict Int Widget
    , nextId : Int
    }

initialModel = { termSource = "\\f.\\g.\\x.f (g x)", widgets = Dict.empty, nextId = 1 }

update msg model =
    case msg of
        SetTermSource s ->
            { model | termSource = s }
        TryAddTerm ->
            case Parser.run LambdaParsers.term model.termSource of
                Err e -> Debug.todo <| Debug.toString e
                Ok term -> addWidget (Widget.initTerm term) model
        RemoveWidget i ->
            { model | widgets = Dict.remove i model.widgets }

        -- additional param: "parent" widget, not relevant yet
        AddEvaluationWidget _ _ ->
            Debug.todo "Not implemented"

        AddTreeWidget term _ ->
            addWidget (Widget.initTree <| LambdaTypes.generateTree term) model

addWidget initialize model =
    { model
        | widgets = Dict.insert
            model.nextId
            (initialize model.nextId)
            model.widgets
        , nextId = model.nextId + 1
    }
