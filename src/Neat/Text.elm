module Neat.Text exposing
    ( Text
    , fromString
    , none
    , setMixin
    , setMixins
    , setAttribute
    , setAttributes
    , when
    , unless
    , withMaybe
    , setNodeName
    , map
    )

{-| Module for inline texts.
This module is for creating special texts. e.g., text that has links only to parts of it.


# Core

@docs Text


# Primitive constructors

@docs fromString
@docs none


# Attributes

@docs setMixin
@docs setMixins
@docs setAttribute
@docs setAttributes


# Handle conditions

@docs when
@docs unless
@docs withMaybe


# Lower level functions for HTML

@docs setNodeName
@docs map

-}

import Html exposing (Attribute)
import Mixin exposing (Mixin)
import Neat.Text.Internal exposing (InlineNode)


{-| Representing a text.
-}
type alias Text msg =
    InlineNode msg



-- Primitive constructors


{-| Generate a `Text` with given text. The result is just a text node but not a HTML node with text content.
Use `setNodeName` or set attributes by following functions to make the result wrapped with HTML tag.

  - setMixin
  - setMixins
  - setAttribute
  - setAttributes

When you use the function above without `setNodeName`, the wrapper HTML tag becomes `span`.

-}
fromString : String -> Text msg
fromString str =
    case str of
        "" ->
            none

        _ ->
            { mixin = Mixin.none
            , nodeName = Nothing
            , text = str
            }


{-| Generate Nothing.
It can be used to realize `Text`s only displayed under certain conditions, as shown below.
In most cases, a function such as `when` will suffice, though.

    fromTexts
        [ case mtext of
            Just t ->
                text t

            Nothing ->
                none
        ]

-}
none : Text msg
none =
    { mixin = Mixin.none
    , nodeName = Nothing
    , text = ""
    }



-- Mixin


{-| -}
setMixin : Mixin msg -> Text msg -> Text msg
setMixin mixin node =
    { node
        | mixin = Mixin.batch [ node.mixin, mixin ]
    }


{-| -}
setMixins : List (Mixin msg) -> Text msg -> Text msg
setMixins mixins node =
    { node
        | mixin = Mixin.batch <| node.mixin :: mixins
    }


{-| -}
setAttribute : Attribute msg -> Text msg -> Text msg
setAttribute attr =
    setMixin <| Mixin.fromAttributes [ attr ]


{-| -}
setAttributes : List (Attribute msg) -> Text msg -> Text msg
setAttributes =
    setMixin << Mixin.fromAttributes



-- Handle conditions


{-| -}
when : Bool -> Text msg -> Text msg
when p v =
    if p then
        v

    else
        none


{-| -}
unless : Bool -> Text msg -> Text msg
unless =
    when << not


{-| -}
withMaybe : Maybe a -> (a -> Text msg) -> Text msg
withMaybe ma f =
    case ma of
        Just a ->
            f a

        Nothing ->
            none



-- Lower level functions for HTML


{-| -}
setNodeName : String -> Text msg -> Text msg
setNodeName str text =
    { text
        | nodeName = Just str
    }


{-| -}
map : (a -> b) -> Text a -> Text b
map f text =
    { mixin = Mixin.map f text.mixin
    , nodeName = text.nodeName
    , text = text.text
    }
