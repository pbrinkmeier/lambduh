module Msg exposing (Msg(..))

import Lambda
import LambdaTypes

type Msg
    = SetTermSource String
    | TryAddTerm
    | RemoveWidget Int
    | AddEvaluationWidget Lambda.Term Int
    | AddTreeWidget Lambda.Term Int
    | AddConstraintsWidget LambdaTypes.Tree Int
