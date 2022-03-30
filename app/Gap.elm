module Gap exposing
    ( Body
    , body
    )

{-|

@docs Body
@docs body

-}

import Neat exposing (IsGap(..))


{-| -}
type Body
    = Body


{-| -}
body : IsGap Body
body =
    IsGap
        { vertical = 0.6
        , horizontal = 0.6
        }
