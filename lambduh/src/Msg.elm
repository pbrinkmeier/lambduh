module Msg exposing (Msg(..))

import Lambda

type Msg
    = SetTermSource String
    | TryAddTerm
    | RemoveWidget Int
    | AddEvaluationWidget Lambda.Term Int
    | AddTreeWidget Lambda.Term Int
