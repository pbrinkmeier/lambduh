module Unification exposing
    ( History
    , unifyStepByStep
    )

import LambdaTypes exposing (Constraint, Type)

type alias History = List HistoryEntry

type alias HistoryEntry =
    { constraints : List Constraint
    , newSubstitution : Maybe Substitution
    , substitutions : List Substitution
    }

type alias Substitution =
    -- TODO: only variables should be on the left side
    -- TODO: split LambdaTypes.Type
    { replace : Type
    , by : Type
    }

unifyStepByStep _ = Debug.todo "bummer"
