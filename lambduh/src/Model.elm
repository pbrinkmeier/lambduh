module Model exposing (Model, Msg(..), initialModel, update)

type alias Model =
    { source : String
    }

type Msg
    = SetSource String

initialModel = { source = "" }

update msg model =
    case msg of
        SetSource s ->
            { model | source = s }
