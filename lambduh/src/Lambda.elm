module Lambda exposing (Term(..))

type Term
    = Var String
    | Abs String Term
    | App Term Term
