module App exposing (main)

import Html
import ForceLayout.Types exposing (Model, Msg)
import ForceLayout.View as View
import ForceLayout.State as State


main : Program Never Model Msg
main =
    Html.program
        { init = State.init
        , update = State.update
        , subscriptions = State.subscriptions
        , view = View.view
        }
