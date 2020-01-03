# lambduh

This folder contains the `lambduh` app.
It is written in Elm, a functional language that compiles to JavaScript.

`lambduh` allows you to evaluate terms in the lambda calculus and to infer their types using the Robinson-algorithm.

## Widgets

A button with a `+` on it will add a widget to the "log".
A widget may contain terms, an interactive evaluator, a type inference tree, constraints gathered from such a tree or an unifier calculated from a set of constraints.

### Technical stuff

The widget system aims to be "kind of FRP-like".
That is, when you update a widget that has been used before to generate another widget, e.g. a set of constraints gathered from a tree, the set of constraints is updated when the tree is updated.
Basically the same system as Excel spreadsheets (or any spreadsheet software, really).

A widget stores some data (such as the lambda term or type tree) and a list of "dependent" widgets that receive a message whenever the widget changes.

### List of widgets and things they can do

- [ ] **Term** containing an editable lambda-term
- [ ] **Tree** containing a **Term**s type tree, editable type context
- [ ] **Constraints** containing the constraints implied by a **Tree**
- [ ] **MGU** containing a most general unifier obtained from a set of constraints
- [ ] **Eval** containing an interactive environment for beta-reducing **Term**s.
