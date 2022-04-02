module Neat.Boundary exposing
    ( Boundary
    , map
    , textBlock
    , fromTexts
    , empty
    , setMixin
    , setMixins
    , setAttribute
    , setAttributes
    , setRole
    , setAria
    , setBoolAria
    , setMinWidthInBs
    , setMinWidthInEm
    , setMinWidthInRem
    , setMinHeightInBs
    , setMinHeightInEm
    , setMinHeightInRem
    , setMaxWidthInfinite
    , setMaxWidthInBs
    , setMaxWidthInEm
    , setMaxWidthInRem
    , setMaxHeightInfinite
    , setMaxHeightInBs
    , setMaxHeightInEm
    , setMaxHeightInRem
    , enableVerticalScroll
    , enableHorizontalScroll
    , setGap
    , setNodeName
    , html
    , htmlNode
    )

{-| Module for building `Boundary`.


# Core

@docs Boundary
@docs map


# Primitive Constructors

@docs textBlock
@docs fromTexts
@docs empty


# Attributes

Functions to set arbitrary attributes.
The following styles are not reflected by the `style` attribute or CSS files.

  - padding / margin
      - Use functions for gaps.
  - min-width / max-width / min-height / max-height / line-height
      - Use functions for sizes.
  - width / height
      - Use min-\* and max-\*.
  - white-space
      - Determined by wrapping style
  - display / position
      - Determined automatically by its context
  - z-index
      - Enforced to be "auto"
  - box-sizing
      - Determined by each type of view.
          - `scalableBlock` is "content-box"
          - otherwise, "border-box"
  - overflow
      - Boundaries automatically handles overflow.
  - position
      - Use putLayer
  - line-height
      - Specify with `textBlock`

@docs setMixin
@docs setMixins
@docs setAttribute
@docs setAttributes


# WAI-ARIA

@docs setRole
@docs setAria
@docs setBoolAria


# Sizes

You can only use a limited kind of units.
This may seem inconvenient, but it prevents you to build unmaintainable broken views.


## minimum width

The initial _minimum width_ is zero without enabling child overflow.

@docs setMinWidthInBs
@docs setMinWidthInEm
@docs setMinWidthInRem


## minimum height

The initial _minimum height_ is zero without enabling child overflow.

@docs setMinHeightInBs
@docs setMinHeightInEm
@docs setMinHeightInRem


## maximum width

The initial value for maximum width is _fit_, which shrinks as much as its children do not overhang it.

@docs setMaxWidthInfinite
@docs setMaxWidthInBs
@docs setMaxWidthInEm
@docs setMaxWidthInRem


## maximum height

The initial value for maximum height is _fit_, which shrinks as much as its children do not overhang it.

@docs setMaxHeightInfinite
@docs setMaxHeightInBs
@docs setMaxHeightInEm
@docs setMaxHeightInRem


# Scroll

@docs enableVerticalScroll
@docs enableHorizontalScroll


# Convert to `View`

@docs setGap


# Lower level functions for HTML

@docs setNodeName
@docs html
@docs htmlNode

-}

import Html exposing (Attribute)
import Mixin exposing (Mixin)
import Neat.Internal as Internal
    exposing
        ( Alignment(..)
        , BaseSize(..)
        , Boundary(..)
        , Boundary_
        , Children(..)
        , Content(..)
        , Gap
        , IsGap(..)
        , Item_
        , MaxHeight(..)
        , MaxWidth(..)
        , MinHeight(..)
        , MinWidth(..)
        , Overlay
        , Size(..)
        , View(..)
        , View_(..)
        )
import Neat.Text as Text exposing (Text)



-- Core


{-| A bounded View without gap.

Convert to/from View by `setGap`/`setBoundary`.

-}
type alias Boundary msg =
    Internal.Boundary msg


defaultBoundary : Boundary_ msg
defaultBoundary =
    { mixin = Mixin.none
    , gap = emptyGap
    , nodeName = "div"
    , innerGap = emptyGap
    , overlays = []
    , width = MinSize
    , minWidth = MinWidthInUnit "" 0
    , maxWidth = MaxWidthFit
    , horizontalOverflow = False
    , height = MinSize
    , minHeight = MinHeightInUnit "" 0
    , maxHeight = MaxHeightFit
    , verticalOverflow = False
    , content = NoContent
    , enforcePointerEvent = False
    }


emptyGap : Gap
emptyGap =
    { vertical = 0
    , horizontal = 0
    }


mapView_ : (a -> b) -> View_ a -> View_ b
mapView_ f view =
    case view of
        FromBoundary boundary ->
            FromBoundary <| mapBoundary_ f boundary

        FromRow o ->
            FromRow
                { mixin = Mixin.map f o.mixin
                , gap = o.gap
                , nodeName = o.nodeName
                , justifyContent = o.justifyContent
                , children = modifyChild (mapView_ f) o.children
                , wrap = o.wrap
                , height = o.height
                , width = o.width
                }

        FromColumn o ->
            FromColumn
                { mixin = Mixin.map f o.mixin
                , gap = o.gap
                , nodeName = o.nodeName
                , justifyContent = o.justifyContent
                , children = modifyChild (mapView_ f) o.children
                , height = o.height
                , width = o.width
                }

        None ->
            None


modifyChild : (View_ a -> View_ b) -> Children a -> Children b
modifyChild f (Children item0 items) =
    let
        modifyContent : Item_ a -> Item_ b
        modifyContent item =
            { alignSelf = item.alignSelf
            , grow = item.grow
            , key = item.key
            , content = f item.content
            }
    in
    List.map modifyContent items
        |> Children (modifyContent item0)


{-| -}
map : (a -> b) -> Boundary a -> Boundary b
map =
    mapBoundary


mapBoundary : (a -> b) -> Boundary a -> Boundary b
mapBoundary f (Boundary boundary) =
    mapBoundary_ f boundary
        |> Boundary


mapBoundary_ : (a -> b) -> Boundary_ a -> Boundary_ b
mapBoundary_ f o =
    { mixin = Mixin.map f o.mixin
    , gap = o.gap
    , nodeName = o.nodeName
    , innerGap = o.innerGap
    , overlays = mapOverlays f o.overlays
    , width = o.width
    , minWidth = o.minWidth
    , maxWidth = o.maxWidth
    , horizontalOverflow = o.horizontalOverflow
    , height = o.height
    , minHeight = o.minHeight
    , maxHeight = o.maxHeight
    , verticalOverflow = o.verticalOverflow
    , content =
        case o.content of
            TextsContent texts ->
                TextsContent <| List.map (Text.map f) texts

            ViewContent view ->
                ViewContent <| mapView_ f view

            HtmlContent children ->
                HtmlContent <|
                    List.map
                        (\( k, b ) ->
                            ( k, mapBoundary_ f b )
                        )
                        children

            StringContent str ->
                StringContent str

            NoContent ->
                NoContent
    , enforcePointerEvent = o.enforcePointerEvent
    }


mapOverlays : (a -> b) -> List (Overlay a) -> List (Overlay b)
mapOverlays f =
    List.map
        (\o ->
            { name = o.name
            , area = o.area
            , boundary = mapBoundary f o.boundary
            }
        )



-- Primitive Constructors


{-| Generates a view that displays a text.

It is an alias for `\str -> fromTexts option [ Neat.Text.fromString str ]`
Note that `textBlock ""` is equivalent to `empty`.

-}
textBlock : String -> Boundary msg
textBlock str =
    fromTexts
        [ Text.fromString str
        ]


{-| An empty block.
-}
empty : Boundary msg
empty =
    Boundary
        { defaultBoundary
            | content = ViewContent None
        }


{-| Build view with text from `Text`s.

Unlike a `Neat.row` of `Neat.textBlock`s, the `fromTexts` generates a single coherent sentence.

For example, the `View` built by the following code will be broken as follows.

    Neat.row
        [ Neat.textBlock "foo bar baz"
        , Neat.textBlock "a b c d e f"
        ]

    | foo bar | a b c d |
    | baz     | e f     |

In contrast, the `View` built by the following code will be broken as follows.

    import Neat.Text as Text

    Neat.fromTexts <| Text.batch
        [ Text.text "foo bar baz"
        , Text.text "a b c d e f"
        ]

| foo bar baz a b c |
| d e f |

The line spacing width when the text is broken is the same as the Gap height applied by the `setGap` function.

-}
fromTexts : List (Text msg) -> Boundary msg
fromTexts ls =
    let
        texts =
            List.filter (\a -> a.text /= "") ls
    in
    Boundary
        { defaultBoundary
            | content =
                if List.isEmpty texts then
                    ViewContent None

                else
                    TextsContent texts
        }



-- Attributes


{-| Append `Mixin` on boundaries.
-}
setMixin : Mixin msg -> Boundary msg -> Boundary msg
setMixin new (Boundary boundary) =
    Boundary { boundary | mixin = Mixin.batch [ boundary.mixin, new ] }


{-| Same as `setMixin` but takes a list of `Mixin`s.
-}
setMixins : List (Mixin msg) -> Boundary msg -> Boundary msg
setMixins ls =
    setMixin <| Mixin.batch ls


{-| Append `Attribute` on boundaries.
-}
setAttribute : Attribute msg -> Boundary msg -> Boundary msg
setAttribute attr =
    setMixin <| Mixin.fromAttributes [ attr ]


{-| Same as `setAttribute` but takes a list of `Attribute`s.
-}
setAttributes : List (Attribute msg) -> Boundary msg -> Boundary msg
setAttributes attrs =
    setMixin <| Mixin.fromAttributes attrs


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> Boundary msg -> Boundary msg
setRole str =
    setMixin <| Mixin.attribute "role" str


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> Boundary msg -> Boundary msg
setAria name v =
    setMixin <| Mixin.attribute ("aria-" ++ name) v


{-| Set boolean "aria-\*" value for WAI-ARIA.

i.e.,

  - `setBoolAria name True` is equal to `setAria name "true"`
  - `setBoolAria name False` is equal to `setAria name "false"`

-}
setBoolAria : String -> Bool -> Boundary msg -> Boundary msg
setBoolAria name g =
    setMixin <| Mixin.boolAttribute ("aria-" ++ name) g



-- Sizing


{-| Set the minimum width as a percentage of the _base size_.
e.g., `setMinWidthInBs 100` set the _minimum width_ the same length as the _base size_.
-}
setMinWidthInBs : Float -> Boundary msg -> Boundary msg
setMinWidthInBs =
    setMinWidth << MinWidthInBs


{-| Set the minimum width in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMinWidthInEm : Float -> Boundary msg -> Boundary msg
setMinWidthInEm =
    setMinWidth << MinWidthInUnit "em"


{-| Set the minimum width in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMinWidthInRem : Float -> Boundary msg -> Boundary msg
setMinWidthInRem =
    setMinWidth << MinWidthInUnit "rem"


setMinWidth : MinWidth -> Boundary msg -> Boundary msg
setMinWidth length (Boundary boundary) =
    Boundary { boundary | minWidth = length }


{-| Set the minimum height as a percentage of the _base size_.
e.g., `setMinHeightInBs 100` set the _minimum height_ the same length as the _base size_.
-}
setMinHeightInBs : Float -> Boundary msg -> Boundary msg
setMinHeightInBs =
    setMinHeight << MinHeightInBs


{-| Set the minimum height in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMinHeightInEm : Float -> Boundary msg -> Boundary msg
setMinHeightInEm =
    setMinHeight << MinHeightInUnit "em"


{-| Set the minimum height in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMinHeightInRem : Float -> Boundary msg -> Boundary msg
setMinHeightInRem =
    setMinHeight << MinHeightInUnit "rem"


setMinHeight : MinHeight -> Boundary msg -> Boundary msg
setMinHeight length (Boundary boundary) =
    Boundary { boundary | minHeight = length }


{-| Set the maximum width to _infinite_, which will be stretched horizontally as much as it does not overhang the parent element.
-}
setMaxWidthInfinite : Boundary msg -> Boundary msg
setMaxWidthInfinite =
    setMaxWidth MaxWidthNone


{-| Set the maximum width as a percentage of the _base size_.
e.g., `setMaxWidthInBs 100` set the _maximum width_ the same length as the _base size_.
-}
setMaxWidthInBs : Float -> Boundary msg -> Boundary msg
setMaxWidthInBs =
    setMaxWidth << MaxWidthInBs


{-| Set the maximum width in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMaxWidthInEm : Float -> Boundary msg -> Boundary msg
setMaxWidthInEm =
    setMaxWidth << MaxWidthInUnit "em"


{-| Set the maximum width in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMaxWidthInRem : Float -> Boundary msg -> Boundary msg
setMaxWidthInRem =
    setMaxWidth << MaxWidthInUnit "rem"


setMaxWidth : MaxWidth -> Boundary msg -> Boundary msg
setMaxWidth length (Boundary boundary) =
    Boundary { boundary | maxWidth = length }


{-| Set the maximum height to _infinite_, which will be stretched vertically as much as it does not overhang the parent element.
-}
setMaxHeightInfinite : Boundary msg -> Boundary msg
setMaxHeightInfinite =
    setMaxHeight MaxHeightNone


{-| Set the maximum height as a percentage of the _base size_.
e.g., `setMaxHeightInBs 100` set the _maximum height_ the same length as the _base size_.
-}
setMaxHeightInBs : Float -> Boundary msg -> Boundary msg
setMaxHeightInBs =
    setMaxHeight << MaxHeightInBs


{-| Set the maximum height in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMaxHeightInEm : Float -> Boundary msg -> Boundary msg
setMaxHeightInEm =
    setMaxHeight << MaxHeightInUnit "em"


{-| Set the maximum height in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMaxHeightInRem : Float -> Boundary msg -> Boundary msg
setMaxHeightInRem =
    setMaxHeight << MaxHeightInUnit "rem"


setMaxHeight : MaxHeight -> Boundary msg -> Boundary msg
setMaxHeight length (Boundary boundary) =
    Boundary { boundary | maxHeight = length }



-- Scroll


{-| -}
enableVerticalScroll : Boundary msg -> Boundary msg
enableVerticalScroll (Boundary boundary) =
    Boundary
        { boundary
            | verticalOverflow = True
        }


{-| -}
enableHorizontalScroll : Boundary msg -> Boundary msg
enableHorizontalScroll (Boundary boundary) =
    Boundary
        { boundary
            | horizontalOverflow = True
        }



-- Convert to `View`


{-| Gap around a view.
-}
setGap : IsGap gap -> Boundary msg -> View gap msg
setGap (IsGap gap) (Boundary boundary) =
    View <|
        case boundary.content of
            NoContent ->
                None

            _ ->
                FromBoundary
                    { boundary
                        | gap = gap
                    }



-- Low level function for HTML


{-| Set HTML node name on `Boundary`.
-}
setNodeName : String -> Boundary msg -> Boundary msg
setNodeName str (Boundary boundary) =
    Boundary { boundary | nodeName = str }


{-| Build HTML tag. When you need to build a HTML tag with its children, `html` will help you.

    animalSelect : Maybe Animal -> Boundary msg
    animalSelect animal =
        Boundary.html "select"
            [ animalOption Nothing animal
            , animalOption (Just Goat) animal
            , animalOption (Just Dog) animal
            , animalOption (Just Cat) animal
            ]
            |> Boundary.setMixin
                (Mixin.Events.onChange ChangeAnimal)

-}
html : String -> List ( String, Boundary msg ) -> Boundary msg
html tag children_ =
    let
        children =
            List.map (\( key, Boundary boundary_ ) -> ( key, boundary_ )) children_
    in
    Boundary
        { defaultBoundary
            | content = HtmlContent children
            , nodeName = tag
        }


{-| A special `Boundary` which only has text content. For example, you can use `htmlNode` to build `option` tag.

    animalOption :
        Maybe Animal
        -> Maybe Animal
        -> ( String, Boundary msg )
    animalOption animal selected =
        let
            key =
                animal
                    |> Maybe.map Animal.toValue
                    |> Maybe.withDefault "default"

            disabled =
                animal == Nothing

            selected =
                animal == selected

            value =
                animal
                    |> Maybe.map Animal.toValue
                    |> Maybe.withDefault ""

            label =
                animal
                    |> Maybe.map Animal.toLabel
                    |> Maybe.withDefault "-- Select one --"
        in
        ( key
        , Boundary.htmlNode "option" label
            |> Boundary.setAttributes
                [ Attributes.disabled disabled
                , Attributes.selected selected
                , Attributes.value value
                ]
        )

-}
htmlNode : String -> String -> Boundary msg
htmlNode tag text =
    Boundary
        { defaultBoundary
            | content = StringContent text
            , nodeName = tag
        }
