module LambdaTypes exposing (Tree, generateTree, viewTree)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Lambda

type alias Tree =
    { context : TypeContext
    , term : Lambda.Term
    , typeId : Int
    , inner : TreeNode
    }

type TreeNode
    = VarNode String Int
    | AbsNode String Tree
    | AppNode Tree Tree

type alias TypeContext = List (String, Int)

ctxLookup k =
    List.foldl (\(ck, i) acc -> if k == ck then Just i else acc) Nothing

generateTree =
    let
        -- n: the variable this term is supposed to be assigned
        -- m: the variable to start indexing subtrees off
        -- this crime against humanity is needed to achieve "natural" numbering in App
        -- **m is not necessarily n + 1**
        -- returns next available variable in tuple (also for App, this is horrible)
        generateTreeN : TypeContext -> Int -> Int -> Lambda.Term -> (Tree, Int)
        generateTreeN ctx n m term =
            case term of
                Lambda.Var varName ->
                    let
                        (id, l) =
                            case ctxLookup varName ctx of
                                Nothing -> (m, m + 1)
                                Just x -> (x, m)
                    in
                    (Tree ctx term n <| VarNode varName id, l)
                Lambda.Abs param body ->
                    let
                        -- todo: insert type
                        (bodyTree, l) = generateTreeN (ctx ++ [(param, m)]) (m + 1) (m + 2) body
                    in
                        (Tree ctx term n <| AbsNode param bodyTree, l)
                Lambda.App f x ->
                    let
                        (fTree, l) = generateTreeN ctx m (m + 2) f
                        (xTree, k) = generateTreeN ctx (m + 1) l x
                    in
                        (Tree ctx term n <| AppNode fTree xTree, k)
    in
        Tuple.first << generateTreeN [] 1 2

viewTree : Tree -> Html a
viewTree { context, term, typeId, inner } =
    let
        stupidViewType id =
            span [ class "type-var" ]
                [ text "α"
                , span [ class "type-var-id" ] [ text <| String.fromInt id ]
                ]

        viewContext =
            let
                viewAss (var, t) =
                    [ span [ class "lambda-id" ] [ text var ]
                    , text " : "
                    , stupidViewType t
                    ]
            in
            List.concat << List.intersperse ([text ", "]) << List.map viewAss

        viewRuleReqs node =
            case node of
                VarNode varName lookupResult ->
                    -- todo
                    List.singleton <| span []
                        <| List.concat
                        <|
                            [ [ text "(" ]
                            , viewContext context
                            , [ text ")("
                              , span [ class "lambda-id" ] [ text varName ]
                              , text ") = "
                              , stupidViewType lookupResult
                              ]
                            ]
                AbsNode _ body ->
                    [ viewTree body ]
                AppNode lhs rhs ->
                    [ viewTree lhs, viewTree rhs ]
    in
        div [ class "rule" ]
            [ div [ class "rule-contents" ]
                [ div [ class "rule-reqs" ] <| List.map (div [ class "rule-req" ] << List.singleton) <| viewRuleReqs inner
                , div [ class "rule-conc" ]
                    [ span [] <| viewContext context
                    , text " ⊢ "
                    , Lambda.viewTerm term
                    , text " : "
                    , stupidViewType typeId
                    ]
                ]
            , div [ class "rule-name" ] [ text <| ruleName inner ]
            ]

ruleName node =
    case node of
        VarNode _ _ -> "Var"
        AbsNode _ _ -> "Abs"
        AppNode _ _ -> "App"
