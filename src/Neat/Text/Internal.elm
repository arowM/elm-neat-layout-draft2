module Neat.Text.Internal exposing (InlineNode)

import Mixin exposing (Mixin)


type alias InlineNode msg =
    { mixin : Mixin msg
    , nodeName : Maybe String
    , text : String
    }
