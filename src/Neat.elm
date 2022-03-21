module Neat exposing
    ( View
    , map
    , NoGap
    , Renderer
    , defaultRenderer
    , setBaseSizeInRem
    , render
    , textBlock
    , fromTexts
    , empty
    , none
    -- , scalableBlock
    -- , textarea
    -- , input
    -- , select
    -- , Options
    , row
    , keyedRow
    , nowrapRow
    , keyedNowrapRow
    , column
    , keyedColumn
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
    , setMaxWidthFit
    , setMaxWidthInBs
    , setMaxWidthInEm
    , setMaxWidthInRem
    , setMaxHeightFit
    , setMaxHeightInBs
    , setMaxHeightInEm
    , setMaxHeightInRem
    , pullChildrenRight
    , pullChildrenHCenter
    , pullChildrenBottom
    , pullChildrenVCenter
    , setGap
    , setBoundary
    -- , setVerticalScroller
    -- , setHorizontalScroller
    -- , putLayer
    , Layer
    , defaultLayer
    -- , Layered
    -- , mapLayered
    -- , toLayered
    -- , when
    -- , unless
    -- , withMaybe
    -- , applyWhen
    -- , applyUnless
    -- , applyWithMaybe
    , IsGap(..)
    , Gap
    , setNodeName
    )

{-| Main module for elm-neat-layout.

# Core

@docs View
@docs map
@docs NoGap


# Render

@docs render
@docs Renderer
@docs defaultRenderer
@docs setBaseSizeInRem


# Primitive constructors

@docs textBlock
@docs fromTexts
@docs empty
@docs none
-- @docs scalableBlock


# Special constructors

-- @docs textarea
-- @docs input
-- @docs select
-- @docs Options


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

The initial value for maximum width is _infinite_, which will be stretched horizontally as much as it does not overhang the parent element.

@docs setMaxWidthFit
@docs setMaxWidthInBs
@docs setMaxWidthInEm
@docs setMaxWidthInRem


## maximum height

The initial value for maximum height is _infinite_, which height be stretched horizontally as much as it does not overhang the parent element.

@docs setMaxHeightFit
@docs setMaxHeightInBs
@docs setMaxHeightInEm
@docs setMaxHeightInRem

# Spacing

## Horizontal alignment

The initial value is _left_, which pulls child elements to the left edge.

@docs pullChildrenRight
@docs pullChildrenHCenter

## Vertical space

The initial value is _top_, which pulls child elements to the top edge.

@docs pullChildrenBottom
@docs pullChildrenVCenter

# Gaps

@docs setGap
@docs setBoundary

# Rows and Columns

@docs row
@docs keyedRow
@docs nowrapRow
@docs keyedNowrapRow
@docs column
@docs keyedColumn


# Overlay

@docs putLayer
@docs Layer
@docs defaultLayer
@docs Layered
@docs mapLayered
@docs toLayered


# Handle conditions

@docs when
@docs unless
@docs withMaybe
@docs applyWhen
@docs applyUnless
@docs applyWithMaybe


# Custom gaps

You can use custom gaps just by declaring new types and `IsGap` values for them.

    import Neat exposing (IsGap)

    type ButtonGroup
        = ButtonGroup

    buttonGroup : IsGap ButtonGroup
    buttonGroup =
        IsGap
            { horizontal = 0.6
            , vertical = 0.6
            }

@docs IsGap
@docs Gap


# Lower level functions for HTML

@docs setNodeName

-}


import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Keyed as Keyed
import Mixin exposing (Mixin)
import Mixin.Html as Mixin
import Neat.Text as Text exposing (Text)
import Neat.Text.Internal exposing (InlineNode)
import Neat.Text as Text exposing (Text)
import Neat.Text.Internal exposing (InlineNode)


-- Core


{-| A gap-sensible `Html msg` alternative.
-}
type View gap msg
    = View (View_ msg)


{-| Internal view.
Some variants have gaps for caching purpose.
-}
type View_ msg
    = Top (View_ msg)
    | Boundary (Boundary_ msg)
    | Scroller (Scroller_ msg)
    | Row (Row_ msg)
    | Column (Column_ msg)
    | TextNode (TextNode_ msg)
    | None


type alias Boundary_ msg =
        { mixin : Mixin msg
        , layout : Layout
        , innerGap : Gap
        , overlays : Overlays msg
        , content : View_ msg
        }


type alias Scroller_ msg =
        { mixin : Mixin msg
        , layout : Layout
        , overlays : Overlays msg
        , content : View_ msg
        , vertical : Bool
        }


type alias Row_ msg =
        { mixin : Mixin msg
        , layout : Layout
        , overlays : Overlays msg
        , children : Children msg
        , wrap : Bool
        }


type alias Column_ msg =
        { mixin : Mixin msg
        , layout : Layout
        , overlays : Overlays msg
        , children : Children msg
        }


type alias TextNode_ msg =
        { mixin : Mixin msg
        , layout : Layout
        , overlays : Overlays msg
        , text : List (Text msg)
        }


type alias Overlays msg =
    List (Overlay msg)


type alias Overlay msg =
    { name : String
    , area : Layer
    , view : View_ msg
    , priority : Maybe Int
    }


mapOverlays : (a -> b) -> Overlays a -> Overlays b
mapOverlays f =
    List.map
        (\o ->
            { name = o.name
            , area = o.area
            , view = map_ f o.view
            , priority = o.priority
            }
        )


type Children msg
    = ChildrenWithKey (String, View_ msg) (List (String, View_ msg))
    | ChildrenWithoutKey (View_ msg) (List (View_ msg))



modifyChild : (View_ a -> View_ b) -> Children a -> Children b
modifyChild f children =
    case children of
        ChildrenWithKey (k0, v0) kvs ->
            List.map (\(k, v) -> (k, f v)) kvs
                |> ChildrenWithKey (k0, f v0)

        ChildrenWithoutKey c cs ->
            List.map f cs
                |> ChildrenWithoutKey (f c)


{-| Set the position of each edge of the overlay layer as a percentage of the base view.

The `priority` field specifies how much the element is superimposed on the front side in preference to other elements. If given `Nothing`, it is set equivalent priority comparing to other elements.
-}
type alias Layer =
    { top : Float
    , bottom : Float
    , left : Float
    , right : Float
    , priority : Maybe Int
    }


{-|

    defaultLayer
    --> { top = 0
    --> , bottom = 0
    --> , left = 0
    --> , right = 0
    --> , priority = Nothing
    --> }

-}
defaultLayer : Layer
defaultLayer =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    , priority = Nothing
    }


{-| -}
map : (a -> b) -> View gap a -> View gap b
map f =
    liftInternal (map_ f)


liftInternal : (View_ a -> View_ b) -> View g1 a -> View g2 b
liftInternal f (View view) =
    View <| f view


map_ : (a -> b) -> View_ a -> View_ b
map_ f view =
    case view of
        Top content ->
            Top (map_ f content)
        Boundary o ->
            Boundary
                { mixin = Mixin.map f o.mixin
                , layout = o.layout
                , innerGap = o.innerGap
                , overlays = mapOverlays f o.overlays
                , content = map_ f o.content
                }

        Scroller o ->
            Scroller
                { mixin = Mixin.map f o.mixin
                , layout = o.layout
                , overlays = mapOverlays f o.overlays
                , content = map_ f o.content
                , vertical = o.vertical
                }

        Row o ->
            Row
                { mixin = Mixin.map f o.mixin
                , layout = o.layout
                , overlays = mapOverlays f o.overlays
                , children = modifyChild (map_ f) o.children
                , wrap = o.wrap
                }

        Column o ->
            Column
                { mixin = Mixin.map f o.mixin
                , layout = o.layout
                , overlays = mapOverlays f o.overlays
                , children = modifyChild (map_ f) o.children
                }

        TextNode o ->
            TextNode
                { mixin = Mixin.map f o.mixin
                , layout = o.layout
                , overlays = mapOverlays f o.overlays
                , text = List.map (Text.map f) o.text
                }

        None ->
            None


type alias Layout =
    { maxWidth : MaxWidth
    , minWidth : MinWidth
    , maxHeight : MaxHeight
    , minHeight : MinHeight
    , alignSelf : Maybe Space
    , verticalSpace : Space
    , horizontalSpace: Space
    , nodeName : String
    , enforcePointerEvent : Bool
    , gap : Gap
    }


defaultLayout : Gap -> Layout
defaultLayout gap =
    { maxWidth = MaxWidthInfinite
    , minWidth = MinWidthInUnit "em" 0
    , maxHeight = MaxHeightInfinite
    , minHeight = MinHeightInUnit "em" 0
    , alignSelf = Nothing
    , verticalSpace = SpaceBehind
    , horizontalSpace = SpaceBehind
    , nodeName = "div"
    , enforcePointerEvent = False
    , gap = gap
    }


extractGap : View_ msg -> Gap
extractGap view =
    case view of
        Top _ -> emptyGap
        Boundary o -> o.layout.gap
        Scroller o -> o.layout.gap
        Row o -> o.layout.gap
        Column o -> o.layout.gap
        TextNode o -> o.layout.gap
        None -> emptyGap


extractMaxWidth : View_ msg -> MaxWidth
extractMaxWidth view =
    case view of
        Top _ -> MaxWidthInfinite
        Boundary o -> o.layout.maxWidth
        Scroller o -> o.layout.maxWidth
        Row o -> o.layout.maxWidth
        Column o -> o.layout.maxWidth
        TextNode o -> o.layout.maxWidth
        None -> MaxWidthFit

extractMaxHeight : View_ msg -> MaxHeight
extractMaxHeight view =
    case view of
        Top _ -> MaxHeightInfinite
        Boundary o -> o.layout.maxHeight
        Scroller o -> o.layout.maxHeight
        Row o -> o.layout.maxHeight
        Column o -> o.layout.maxHeight
        TextNode o -> o.layout.maxHeight
        None -> MaxHeightFit

{-| A primitive type that represents that there is no Gap

For custom gaps, see [Custom gaps](#custom-gaps).

-}
type NoGap
    = NoGap


emptyGap : Gap
emptyGap =
    { vertical = 0
    , horizontal = 0
    }


-- Custom gaps


{-| Information about your custom gaps.

  - horizontal : horizontal gap relative to _base size_
  - vertical : vertical gap relative to _base size_

e.g., If the _base size_ is `2rem`, `IsGap { horizontal = 1.2, vertical = 2 }` becomes gap with `"2.4rem"` horizontally and `"4rem"` vertically.

-}
type IsGap p
    = IsGap Gap


{-| -}
type alias Gap =
    { horizontal : Float
    , vertical : Float
    }


subtractGap : Gap -> Gap -> Gap
subtractGap g2 g1 =
    { horizontal = g1.horizontal - g2.horizontal
    , vertical = g1.vertical - g2.vertical
    }





-- Internal Model


{-| -}
type Model model
    = Model (Model_ model)


type alias Model_ model =
    { userModel : model
    }


init : ( model, Cmd msg ) -> ( Model model, Cmd (Msg msg) )
init ( userModel, userCmd ) =
    ( Model
        { userModel = userModel
        }
    , Cmd.map UpdateUserMsg userCmd
    )


-- Internal Message


{-| -}
type Msg msg
    = UpdateUserMsg msg


update : (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
update userUpdate msg (Model model) =
    update_ userUpdate msg model
        |> (\( newModel, cmd ) ->
                ( Model newModel
                , cmd
                )
           )


update_ : (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model_ model -> ( Model_ model, Cmd (Msg msg) )
update_ userUpdate msg model =
    case msg of
        UpdateUserMsg userMsg ->
            userUpdate userMsg model.userModel
                |> (\( userModel, userCmd ) ->
                        ( { model
                            | userModel = userModel
                          }
                        , Cmd.map UpdateUserMsg userCmd
                        )
                   )


-- `Browser.*` alternatives

{-| Settings for rendering `View`.
-}
type Renderer
    = Renderer Renderer_


type alias Renderer_ =
    { baseSize : BaseSize
    , debug : Bool
    }


type BaseSize
    = BaseSize String Float


mapBaseSize : (Float -> Float) -> BaseSize -> BaseSize
mapBaseSize f (BaseSize unit a) =
    BaseSize unit (f a)


multipleBaseSize : Float -> BaseSize -> BaseSize
multipleBaseSize a =
    mapBaseSize (\s -> a * s)


renderBaseSize : BaseSize -> String
renderBaseSize (BaseSize unit a) =
    String.concat
        [ String.fromFloat a
        , unit
        ]


{-|

  - base size: 1rem

-}
defaultRenderer : Renderer
defaultRenderer =
    Renderer defaultRenderer_


{-| Set the base size in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

All gap sizes are determined relative to this value.

-}
setBaseSizeInRem : Float -> Renderer -> Renderer
setBaseSizeInRem a (Renderer renderer) =
    Renderer <|
        { renderer
            | baseSize = BaseSize "rem" a
        }


defaultRenderer_ : Renderer_
defaultRenderer_ =
    { baseSize = BaseSize "rem" 1
    , debug = False
    }


-- Primitive nodes


{-| Generates a view that displays a text.

It is an alias for `\str -> fromTexts option [ Neat.Text.fromString str ]`
Note that `textBlock ""` is equivalent to `none`.

-}
textBlock : IsGap gap -> String -> View gap msg
textBlock gap str =
    fromTexts gap
        [ Text.fromString str
        ]



{-| An empty block.
-}
empty : View NoGap msg
empty =
            Boundary
                { mixin = Mixin.none
                , layout = defaultLayout emptyGap
                , overlays = []
                , innerGap = emptyGap
                , content = None
                }
                    |> View


{-| Build view with text from `Text`s.
The _maximum height_ is set to _fit_.

Unlike a `Neat.row` of `Neat.textBlock`s, the `fromTexts` generates a single coherent sentence.

For example, the `View` generated with the following code will be broken as follows.

    Neat.row
        [ Neat.textBlock "foo bar baz"
        , Neat.textBlock "a b c d e f"
        ]

    | foo bar | a b c d |
    | baz     | e f     |

In contrast, the `View` generated by the following code will be broken as follows.

    import Neat.Text as Text

    Neat.fromText <| Text.batch
        [ Text.text "foo bar baz"
        , Text.text "a b c d e f"
        ]

| foo bar baz a b c |
| d e f |

The given gap also affects the line spacing when the string breaks.
-}
fromTexts : IsGap gap -> List (Text msg) -> View gap msg
fromTexts (IsGap gap) ls =
    let
        texts = List.filter (\a -> a.text /= "") ls
    in
    case texts of
        [] -> none
        _ ->
            TextNode
                { mixin = Mixin.none
                , layout =
                    let
                        layout = defaultLayout gap
                    in
                    { layout
                        | maxHeight = MaxHeightFit
                    }
                , overlays = []
                , text = texts
                }
                |> View


{-| Generates no HTML nodes.
This is useful for handling elements which only appears under certain conditions.
-}
none : View g a
none =
    View None


{-| Align children horizontally.
-}
row : List (View p msg) -> View p msg
row = row_ True


{-| Like `row`, but it cannot wrap.
-}
nowrapRow : List (View p msg) -> View p msg
nowrapRow = row_ False


row_ : Bool -> List (View p msg) -> View p msg
row_ wrap children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\(View c) ->
                        case c of
                            None ->
                                Nothing

                            _ ->
                                Just c
                    )
    in
    View <|
        case children of
            [] ->
                None

            (c :: cs) ->
                Row
                    { mixin = Mixin.none
                    , layout = defaultLayout <| extractGap c
                    , overlays = []
                    , children = ChildrenWithoutKey c cs
                    , wrap = wrap
                    }


{-| Works just like `row`, but you add a unique identifier to each child node. You want this when you have a list of nodes that is changing: adding nodes, removing nodes, etc. In these cases, the unique identifiers help make the DOM modifications more efficient.
-}
keyedRow : List ( String, View gap msg ) -> View gap msg
keyedRow = keyedRow_ True


{-| Works just like `nowrapRow`, but you add a unique identifier to each child node. You want this when you have a list of nodes that is changing: adding nodes, removing nodes, etc. In these cases, the unique identifiers help make the DOM modifications more efficient.
-}
keyedNowrapRow : List ( String, View gap msg ) -> View gap msg
keyedNowrapRow = keyedRow_ False


keyedRow_ : Bool -> List ( String, View gap msg ) -> View gap msg
keyedRow_ wrap children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\( str, View c ) ->
                        case c of
                            None ->
                                Nothing

                            _ ->
                                Just ( str, c )
                    )
    in
    View <|
        case children of
            [] ->
                None

            ((k0, v0) :: kvs) ->
                Row
                    { mixin = Mixin.none
                    , layout = defaultLayout <| extractGap v0
                    , overlays = []
                    , children =
                        ChildrenWithKey (k0, v0) kvs
                    , wrap = wrap
                    }


{-| Align children vertically.
-}
column : List (View p msg) -> View p msg
column children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\(View c) ->
                        case c of
                            None ->
                                Nothing

                            _ ->
                                Just c
                    )
    in
    View <|
        case children of
            [] ->
                None

            (c :: cs) ->
                Column
                    { mixin = Mixin.none
                    , layout = defaultLayout <| extractGap c
                    , overlays = []
                    , children = ChildrenWithoutKey c cs
                    }


{-| Works just like `column`, but you add a unique identifier to each child node. You want this when you have a list of nodes that is changing: adding nodes, removing nodes, etc. In these cases, the unique identifiers help make the DOM modifications more efficient.
-}
keyedColumn : List ( String, View gap msg ) -> View gap msg
keyedColumn children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\( str, View c ) ->
                        case c of
                            None ->
                                Nothing

                            _ ->
                                Just ( str, c )
                    )
    in
    View <|
        case children of
            [] ->
                None

            ((k0, v0) :: kvs) ->
                Column
                    { mixin = Mixin.none
                    , layout = defaultLayout <| extractGap v0
                    , overlays = []
                    , children = ChildrenWithKey (k0, v0) kvs
                    }


-- Spaces


{-| It has no effect on _fit_ views.
-}
type Space
    = SpaceBehind
    | SpaceForward
    | SpaceBoth


-- Setter


{-| Set `Mixin` on views with no gaps.
-}
setMixin : Mixin msg -> View NoGap msg -> View NoGap msg
setMixin mixin =
    liftInternal (setMixin_ mixin)


setMixin_ : Mixin msg -> View_ msg -> View_ msg
setMixin_ new view =
    case view of
        Top ls ->
            Top ls

        Boundary o ->
            Boundary { o | mixin = Mixin.batch [ o.mixin, new ] }

        Scroller o ->
            Scroller { o | mixin = Mixin.batch [ o.mixin, new ] }

        Row o ->
            Row { o | mixin = Mixin.batch [ o.mixin, new ] }

        Column o ->
            Column { o | mixin = Mixin.batch [ o.mixin, new ] }

        TextNode o ->
            TextNode { o | mixin = Mixin.batch [ o.mixin, new ] }

        None ->
            None


{-| Same as `setMixin` but takes list of `Mixin`s.
-}
setMixins : List (Mixin msg) -> View NoGap msg -> View NoGap msg
setMixins ls =
    setMixin <| Mixin.batch ls


{-| Set `Attribute` on views without gaps.
-}
setAttribute : Attribute msg -> View NoGap msg -> View NoGap msg
setAttribute attr =
    setMixin <| Mixin.fromAttribute attr


{-| Same as `setAttribute` but takes list of `Attribute`s.
-}
setAttributes : List (Attribute msg) -> View NoGap msg -> View NoGap msg
setAttributes attrs =
    setMixin <| Mixin.fromAttributes attrs


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> View g msg -> View g msg
setRole v =
    liftInternal (setMixin_ <| Mixin.attribute "role" v)


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> View g msg -> View g msg
setAria name v =
    liftInternal (setMixin_ <| Mixin.attribute ("aria-" ++ name) v)


{-| Set boolean "aria-\*" value for WAI-ARIA.

i.e.,

  - `setBoolAria name True` is equal to `setAria name "true"`
  - `setBoolAria name False` is equal to `setAria name "false"`

-}
setBoolAria : String -> Bool -> View g msg -> View g msg
setBoolAria name g =
    setAria name <|
        if g then
            "true"

        else
            "false"




-- Sizing


{-| -}
type MinWidth
    = MinWidthInBs Float
    | MinWidthInUnit String Float

{-| -}
type MaxWidth
    = MaxWidthInfinite
    | MaxWidthFit
    | MaxWidthInBs Float
    | MaxWidthInUnit String Float


{-| -}
type MinHeight
    = MinHeightInBs Float
    | MinHeightInUnit String Float

{-| -}
type MaxHeight
    = MaxHeightInfinite
    | MaxHeightFit
    | MaxHeightInBs Float
    | MaxHeightInUnit String Float


{-| Set the minimum width as a percentage of the _base size_.
e.g., `setMinWidthInBs 100` set the _minimum width_ the same length as the _base size_.
-}
setMinWidthInBs : Float -> View NoGap msg -> View NoGap msg
setMinWidthInBs =
    setMinWidth << MinWidthInBs


{-| Set the minimum width in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMinWidthInEm : Float -> View NoGap msg -> View NoGap msg
setMinWidthInEm =
    setMinWidth << MinWidthInUnit "em"


{-| Set the minimum width in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMinWidthInRem : Float -> View NoGap msg -> View NoGap msg
setMinWidthInRem =
    setMinWidth << MinWidthInUnit "rem"


setMinWidth : MinWidth -> View g msg -> View g msg
setMinWidth length =
    modifyLayout <|
        \layout ->
            { layout
                | minWidth = length
            }

{-| Set the minimum height as a percentage of the _base size_.
e.g., `setMinHeightInBs 100` set the _minimum height_ the same length as the _base size_.

-}
setMinHeightInBs : Float -> View NoGap msg -> View NoGap msg
setMinHeightInBs =
    setMinHeight << MinHeightInBs


{-| Set the minimum height in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMinHeightInEm : Float -> View NoGap msg -> View NoGap msg
setMinHeightInEm =
    setMinHeight << MinHeightInUnit "em"


{-| Set the minimum height in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMinHeightInRem : Float -> View NoGap msg -> View NoGap msg
setMinHeightInRem =
    setMinHeight << MinHeightInUnit "rem"


setMinHeight : MinHeight -> View g msg -> View g msg
setMinHeight length =
    modifyLayout <|
        \layout ->
            { layout
                | minHeight = length
            }


{-| Set the maximum width to _fit_, which shrinks to its minimum width.

If this is applied, the maximum width of all child views are set to _fit_.

-}
setMaxWidthFit : View NoGap msg -> View NoGap msg
setMaxWidthFit =
    modifyLayout <|
        \layout ->
            { layout | maxWidth = MaxWidthFit }
-- setMaxWidthFit (View view) =
--     View <| setMaxWidthFit_ view


setMaxWidthFit_ : View_ msg -> View_ msg
setMaxWidthFit_ view =
    let
        helper : Layout -> Layout
        helper layout =
            { layout | maxWidth = MaxWidthFit }
    in
    case view of
        Top c ->
            Top <| setMaxWidthFit_ c

        Boundary o ->
            Boundary
                { o
                    | layout = helper o.layout
                    , content =
                        setMaxWidthFit_ o.content
                }

        Scroller o ->
            Scroller
                { o
                    | layout = helper o.layout
                    , content =
                        setMaxWidthFit_ o.content
                }

        Row o ->
            Row
                { o
                    | layout = helper o.layout
                    , children =
                        modifyChild setMaxWidthFit_ o.children
                }

        Column o ->
            Column
                { o
                    | layout = helper o.layout
                    , children =
                        modifyChild setMaxWidthFit_ o.children
                }

        TextNode o ->
            TextNode
                { o | layout = helper o.layout }

        None ->
            None



{-| Set the maximum width as a percentage of the _base size_.
e.g., `setMaxWidthInBs 100` set the _maximum width_ the same length as the _base size_.

-}
setMaxWidthInBs : Float -> View NoGap msg -> View NoGap msg
setMaxWidthInBs =
    setMaxWidth << MaxWidthInBs


{-| Set the maximum width in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMaxWidthInEm : Float -> View NoGap msg -> View NoGap msg
setMaxWidthInEm =
    setMaxWidth << MaxWidthInUnit "em"


{-| Set the maximum width in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMaxWidthInRem : Float -> View NoGap msg -> View NoGap msg
setMaxWidthInRem =
    setMaxWidth << MaxWidthInUnit "rem"


setMaxWidth : MaxWidth -> View g msg -> View g msg
setMaxWidth length =
    modifyLayout <|
        \layout ->
            { layout
                | maxWidth = length
            }


{-| Set the maximum height to _fit_, which shrinks to its minimum height.

If this is applied, the maximum height of all child views are set to _fit_.

-}
setMaxHeightFit : View NoGap msg -> View NoGap msg
setMaxHeightFit (View view) =
    View <| setMaxHeightFit_ view


setMaxHeightFit_ : View_ msg -> View_ msg
setMaxHeightFit_ view =
    let
        helper : Layout -> Layout
        helper layout =
            { layout | maxHeight = MaxHeightFit }
    in
    case view of
        Top c ->
            Top <| setMaxHeightFit_ c

        Boundary o ->
            Boundary
                { o
                    | layout = helper o.layout
                    , content =
                        setMaxHeightFit_ o.content
                }

        Scroller o ->
            Scroller
                { o
                    | layout = helper o.layout
                    , content =
                        setMaxHeightFit_ o.content
                }

        Row o ->
            Row
                { o
                    | layout = helper o.layout
                    , children =
                        modifyChild setMaxHeightFit_ o.children
                }

        Column o ->
            Column
                { o
                    | layout = helper o.layout
                    , children =
                        modifyChild setMaxHeightFit_ o.children
                }

        TextNode o ->
            TextNode
                { o | layout = helper o.layout }

        None ->
            None


{-| Set the maximum height as a percentage of the _base size_.
e.g., `setMaxHeightInBs 100` set the _maximum height_ the same length as the _base size_.

-}
setMaxHeightInBs : Float -> View NoGap msg -> View NoGap msg
setMaxHeightInBs =
    setMaxHeight << MaxHeightInBs


{-| Set the maximum height in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMaxHeightInEm : Float -> View NoGap msg -> View NoGap msg
setMaxHeightInEm =
    setMaxHeight << MaxHeightInUnit "em"


{-| Set the maximum height in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMaxHeightInRem : Float -> View NoGap msg -> View NoGap msg
setMaxHeightInRem =
    setMaxHeight << MaxHeightInUnit "rem"


setMaxHeight : MaxHeight -> View g msg -> View g msg
setMaxHeight length =
    modifyLayout <|
        \layout ->
            { layout
                | maxHeight = length
            }




modifyLayout : (Layout -> Layout) -> View g1 msg -> View g2 msg
modifyLayout f =
    liftInternal (modifyLayout_ f)


modifyLayout_ : (Layout -> Layout) -> View_ msg -> View_ msg
modifyLayout_ f view =
    case view of
        Top c -> Top c
        Boundary o ->
            Boundary { o | layout = f o.layout }
        Scroller o ->
            Scroller { o | layout = f o.layout }
        Row o ->
            Row { o | layout = f o.layout }

        Column o ->
            Column { o | layout = f o.layout }

        TextNode o ->
            TextNode { o | layout = f o.layout }

        None ->
            None


-- Spacing


{-| Pull child elements to the right edge.
-}
pullChildrenRight : View gap msg -> View gap msg
pullChildrenRight =
    modifyLayout <|
        \layout ->
            { layout
                | horizontalSpace = SpaceForward
            }


{-| Center child elements horizontally.
-}
pullChildrenHCenter : View gap msg -> View gap msg
pullChildrenHCenter =
    modifyLayout <|
        \layout ->
            { layout
                | horizontalSpace = SpaceBoth
            }


{-| Pull child elements to the bottom edge.
-}
pullChildrenBottom : View gap msg -> View gap msg
pullChildrenBottom =
    modifyLayout <|
        \layout ->
            { layout
                | verticalSpace = SpaceForward
            }


{-| Center child elements vertically.
-}
pullChildrenVCenter : View gap msg -> View gap msg
pullChildrenVCenter =
    modifyLayout <|
        \layout ->
            { layout
                | verticalSpace = SpaceBoth
            }



-- Gap


{-| Gap around a view.
-}
setGap : IsGap new -> View NoGap msg -> View new msg
setGap (IsGap gap) (View view) =
    setGap_ gap view
        |> View


setGap_ : Gap -> View_ msg -> View_ msg
setGap_ gap view =
    let
        helper : Layout -> Layout
        helper layout = { layout | gap = gap }
    in
    case view of
        Top c ->
            Top c

        Boundary o ->
            Boundary
                { o | layout = helper o.layout }

        Scroller o ->
            Scroller
                { o | layout = helper o.layout }

        Row o ->
            Row
                { o | layout = helper o.layout }

        Column o ->
            Column
                { o | layout = helper o.layout }

        TextNode o ->
            TextNode { o | layout = helper o.layout }

        None ->
            None


{-| Wrap a view with boundary without gap.
This is the only way to reset view gaps.
-}
setBoundary : View gap msg -> View NoGap msg
setBoundary (View view) =
    case view of
        None -> View None
        _ ->
            Boundary
                { mixin = Mixin.none
                , layout = defaultLayout emptyGap
                , overlays = []
                , innerGap = extractGap view
                , content = view
                }
                    |> View


{-| Wrap a view with special boundary, which enables vertical scroll.
-}
setVerticalScroller : View NoGap msg -> View NoGap msg
setVerticalScroller (View view) =
    case view of
        None -> View None
        _ ->
            Scroller
                { mixin = Mixin.none
                , layout = defaultLayout emptyGap
                , overlays = []
                , content = view
                , vertical = True
                }
                    |> View

{-| Wrap a view with special boundary, which enables horizontal scroll.
-}
setHorizontalScroller : View NoGap msg -> View NoGap msg
setHorizontalScroller (View view) =
    case view of
        None -> View None
        _ ->
            Scroller
                { mixin = Mixin.none
                , layout = defaultLayout emptyGap
                , overlays = []
                , content = view
                , vertical = False
                }
                    |> View

-- Render


{-| Render the `View` into `Html` so that it spreads across the screen.
-}
render : Renderer -> View NoGap msg -> Html msg
render (Renderer renderer) (View v) =
    Top v
        |> render_ renderer Mixin.none


render_ : Renderer_ -> Mixin msg -> View_ msg -> Html msg
render_ renderer extra view =
    case view of
        Top c ->
            renderTop renderer extra c

        Boundary o ->
            renderBoundary renderer extra o

        Scroller o ->
            renderScroller renderer extra o

        Row o ->
            renderRow renderer extra o

        Column o ->
            renderColumn renderer extra o

        TextNode o ->
            renderTextNode renderer extra o

        None ->
            Html.text ""


renderTop : Renderer_ -> Mixin msg -> View_ msg -> Html msg
renderTop renderer extra c =
    let
        childMixin =
                Mixin.batch
                    [ extra
                    , class "top"
                    , childClass "top" c
                    ]
    in
    render_ renderer childMixin c


renderBoundary : Renderer_ -> Mixin msg -> Boundary_ msg -> Html msg
renderBoundary renderer extra o =
    let
        childMixin =
            Mixin.batch
                [ class "boundaryContent"
                , childClass "boundaryContent" o.content
                , if requireBefore then
                    class "boundaryContent-requireBefore"
                  else
                    Mixin.none
                ]
        requireBefore =
            o.layout.maxHeight /= MaxHeightFit
    in
    Mixin.lift (Html.node o.layout.nodeName)
        [ boundaryCustomProperty renderer o
        , extra
        , boundaryClass { requireBefore = requireBefore } renderer o
        ]
        [ Mixin.div
            [ o.mixin
            , class "boundary_core"
            ]
            [ render_ renderer childMixin o.content
            ]
        ]




boundaryCustomProperty : Renderer_ -> Boundary_ msg -> Mixin msg
boundaryCustomProperty renderer o =
    customProperty <|
        (commonCustomProperty renderer o.layout ++
            [ ( "--inner-gap-x"
              , multipleBaseSize o.innerGap.horizontal renderer.baseSize
                      |> renderBaseSize
              )
            , ( "--inner-gap-y"
              , multipleBaseSize o.innerGap.vertical renderer.baseSize
                      |> renderBaseSize
              )
            ]
        )


boundaryClass : { requireBefore : Bool } -> Renderer_ -> Boundary_ msg -> Mixin msg
boundaryClass { requireBefore } renderer o =
    Mixin.batch
        [ class "boundary"
        , commonClass "boundary" o.layout
        , if requireBefore then
            class "boundary-requireBefore"
          else
            Mixin.none
        ]


renderScroller : Renderer_ -> Mixin msg -> Scroller_ msg -> Html msg
renderScroller renderer extra o =
    let
        childMixin =
            Mixin.batch
                [ class "scrollerContent"
                , childClass "scrollerContent" o.content
                , if o.vertical then
                    class "scrollerContent-vertical"
                  else
                    class "scrollerContent-horizontal"
                ]
        requireBefore =
            o.layout.maxHeight /= MaxHeightFit
    in
    Mixin.lift (Html.node o.layout.nodeName)
        [ scrollerCustomProperty renderer o
        , extra
        , scrollerClass { requireBefore = requireBefore } renderer o
        ]
        [ Mixin.div
            [ o.mixin
            , class "scroller_core"
            ]
            [ render_ renderer childMixin o.content
            ]
        ]




scrollerCustomProperty : Renderer_ -> Scroller_ msg -> Mixin msg
scrollerCustomProperty renderer o =
    customProperty <|
        commonCustomProperty renderer o.layout


scrollerClass : { requireBefore : Bool } -> Renderer_ -> Scroller_ msg -> Mixin msg
scrollerClass { requireBefore } renderer o =
    Mixin.batch
        [ class "scroller"
        , commonClass "scroller" o.layout
        , if requireBefore then
            class "scroller-requireBefore"
          else
            Mixin.none
        , if o.vertical then
            class "scroller-vertical"
          else
            class "scroller-horizontal"
        ]


renderRow : Renderer_ -> Mixin msg -> Row_ msg -> Html msg
renderRow renderer extra o =
    case o.children of
        ChildrenWithoutKey c cs ->
            let
                single =
                    List.isEmpty cs
                requireBefore =
                    single && o.layout.maxHeight /= MaxHeightFit
                flex =
                    (not single) &&
                    (o.layout.maxWidth /= MaxWidthFit) &&
                    (List.any (\v -> extractMaxWidth v == MaxWidthInfinite) (c::cs))
                childMixin c1 =
                    if flex then
                        Mixin.batch
                            [ class "rowChild-flex"
                            , childClass "rowChild-flex" c1
                            ]
                    else
                        Mixin.batch
                            [ class "rowChild-inline"
                            , childClass "rowChild-inline" c1
                            ]
            in
            (c :: cs)
                |> List.map (\c1 -> render_ renderer (childMixin c1) c1)
                |> Mixin.lift (Html.node o.layout.nodeName)
                    [ rowCustomProperty renderer o
                    , o.mixin
                    , extra
                    , rowClass
                        { requireBefore = requireBefore
                        , flex = flex
                        }
                        renderer o
                    ]
        ChildrenWithKey kv kvs ->
            let
                single =
                    List.isEmpty kvs
                requireBefore =
                    single && o.layout.maxHeight /= MaxHeightFit
                flex =
                    (not single) &&
                    (o.layout.maxWidth /= MaxWidthFit) &&
                    (List.any (\(_, v) -> extractMaxWidth v == MaxWidthInfinite) (kv::kvs))
                childMixin c1 =
                    if flex then
                        Mixin.batch
                            [ class "rowChild-flex"
                            , childClass "rowChild-flex" c1
                            ]
                    else
                        Mixin.batch
                            [ class "rowChild-inline"
                            , childClass "rowChild-inline" c1
                            ]
            in
            (kv :: kvs)
                |> List.map
                    (\(k, v) ->
                        ( k
                        , render_ renderer (childMixin v) v
                        )
                    )
                |> Keyed.node o.layout.nodeName
                    (Mixin.toAttributes <| Mixin.batch
                        [ rowCustomProperty renderer o
                        , o.mixin
                        , extra
                        , rowClass
                            { requireBefore = requireBefore
                            , flex = flex
                            }
                            renderer o
                        ]
                    )


rowCustomProperty : Renderer_ -> Row_ msg -> Mixin msg
rowCustomProperty renderer o =
    customProperty
        <| commonCustomProperty renderer o.layout


rowClass : { flex : Bool, requireBefore : Bool } -> Renderer_ -> Row_ msg -> Mixin msg
rowClass { flex, requireBefore} renderer o =
    Mixin.batch
        [ class "row"
        , if o.wrap then
            Mixin.none
          else
            class "row-nowrap"
        , if flex then
            Mixin.batch
                [ class "row-flex"
                , commonClass "row-flex" o.layout
                ]
          else
            Mixin.batch
                [ class "row-inline"
                , commonClass "row-inline" o.layout
                ]
        , if requireBefore then
            class "row-requireBefore"
          else
            Mixin.none
        ]


renderColumn : Renderer_ -> Mixin msg -> Column_ msg -> Html msg
renderColumn renderer extra o =
    case o.children of
        ChildrenWithoutKey c cs ->
            let
                single =
                    List.isEmpty cs
                flex =
                    (o.layout.verticalSpace /= SpaceBehind) ||
                    ( (not single) &&
                      (o.layout.maxHeight /= MaxHeightFit) &&
                      (List.any (\v -> extractMaxHeight v == MaxHeightInfinite) (c::cs))
                    )
                childMixin c1 =
                    if flex then
                        Mixin.batch
                            [ class "columnChild-flex"
                            , childClass "columnChild-flex" c1
                            ]
                    else
                        Mixin.batch
                            [ class "columnChild-block"
                            , childClass "columnChild-block" c1
                            ]
            in
            (c :: cs)
                |> List.map (\c1 -> render_ renderer (childMixin c1) c1)
                |> Mixin.lift (Html.node o.layout.nodeName)
                    [ columnCustomProperty renderer o
                    , o.mixin
                    , extra
                    , columnClass
                        { flex = flex
                        }
                        renderer o
                    ]
        ChildrenWithKey kv kvs ->
            let
                single =
                    List.isEmpty kvs
                flex =
                    (not single) &&
                    (o.layout.maxHeight /= MaxHeightFit) &&
                    (List.any (\(_, v) -> extractMaxHeight v == MaxHeightInfinite) (kv::kvs))
                childMixin c1 =
                    if flex then
                        Mixin.batch
                            [ class "columnChild-flex"
                            , childClass "columnChild-flex" c1
                            ]
                    else
                        Mixin.batch
                            [ class "columnChild-block"
                            , childClass "columnChild-block" c1
                            ]
            in
            (kv :: kvs)
                |> List.map
                    (\(k, v) ->
                        ( k
                        , render_ renderer (childMixin v) v
                        )
                    )
                |> Keyed.node o.layout.nodeName
                    (Mixin.toAttributes <| Mixin.batch
                        [ columnCustomProperty renderer o
                        , o.mixin
                        , extra
                        , columnClass
                            { flex = flex
                            }
                            renderer o
                        ]
                    )


columnCustomProperty : Renderer_ -> Column_ msg -> Mixin msg
columnCustomProperty renderer o =
    customProperty
        <| commonCustomProperty renderer o.layout


columnClass : { flex : Bool } -> Renderer_ -> Column_ msg -> Mixin msg
columnClass { flex } renderer o =
    Mixin.batch
        [ class "column"
        , if flex then
            Mixin.batch
                [ class "column-flex"
                , commonClass "column-flex" o.layout
                ]
          else
            Mixin.batch
                [ class "column-block"
                , commonClass "column-block" o.layout
                ]
        ]


renderTextNode : Renderer_ -> Mixin msg -> TextNode_ msg -> Html msg
renderTextNode renderer extra o =
    let
        requireBefore =
            o.layout.maxHeight /= MaxHeightFit
    in
    Mixin.lift (Html.node o.layout.nodeName)
        [ textNodeCustomProperty renderer o
        , extra
        , textNodeClass { requireBefore = requireBefore } renderer o
        ]
        [ o.text
            |> List.map
                (\inline ->
                    Mixin.lift (Html.node inline.nodeName)
                        [ inline.mixin
                        , class "textNode_text_inline"
                        ]
                        [ Html.text inline.text
                        ]
                )
            |> Mixin.span
                [ o.mixin
                , class "textNode_text"
                ]
        ]


textNodeCustomProperty : Renderer_ -> TextNode_ msg -> Mixin msg
textNodeCustomProperty renderer o =
    customProperty <|
        commonCustomProperty renderer o.layout


textNodeClass : { requireBefore : Bool } -> Renderer_ -> TextNode_ msg -> Mixin msg
textNodeClass { requireBefore } renderer o =
    Mixin.batch
        [ class "textNode"
        , if requireBefore then
            class "textNode-requireBefore"
          else
            Mixin.none
        , commonClass "textNode" o.layout
        ]


commonCustomProperty : Renderer_ -> Layout -> List (String, String)
commonCustomProperty renderer layout =
    List.concat
        [ case layout.minWidth of
            MinWidthInBs bs ->
                [ ( "--min-width"
                  , multipleBaseSize bs renderer.baseSize
                    |> renderBaseSize
                  )
                ]
            MinWidthInUnit unit v ->
                [ ( "--min-width"
                  , String.fromFloat v ++ unit
                  )
                ]
        , case layout.maxWidth of
            MaxWidthInBs bs ->
                [ ( "--max-width"
                  , multipleBaseSize bs renderer.baseSize
                    |> renderBaseSize
                  )
                ]

            MaxWidthInUnit unit v ->
                [ ( "--max-width"
                  , String.fromFloat v ++ unit
                  )
                ]
            _ ->
                []
        , case layout.minHeight of
            MinHeightInBs bs ->
                [ ( "--min-height"
                  , multipleBaseSize bs renderer.baseSize
                    |> renderBaseSize
                  )
                ]
            MinHeightInUnit unit v ->
                [ ( "--min-height"
                  , String.fromFloat v ++ unit
                  )
                ]
        , case layout.maxHeight of
            MaxHeightInBs bs ->
                [ ( "--max-height"
                  , multipleBaseSize bs renderer.baseSize
                    |> renderBaseSize
                  )
                ]

            MaxHeightInUnit unit v ->
                [ ( "--max-height"
                  , String.fromFloat v ++ unit
                  )
                ]
            _ ->
                []
        , [ ( "--outer-gap-x"
            , multipleBaseSize layout.gap.horizontal renderer.baseSize
                    |> renderBaseSize
            )
          , ( "--outer-gap-y"
            , multipleBaseSize layout.gap.vertical renderer.baseSize
                    |> renderBaseSize
            )
          ]
        ]

commonClass : String -> Layout -> Mixin msg
commonClass prefix layout =
    let
        localClass str = class <| prefix ++ "-" ++ str
    in
    Mixin.batch
        [ case layout.alignSelf of
            Just SpaceBehind ->
                localClass "alignSelfStart"
            Just SpaceForward ->
                localClass "alignSelfEnd"
            Just SpaceBoth ->
                localClass "alignSelfCenter"
            _ ->
                Mixin.none
        , case layout.verticalSpace of
            SpaceBehind ->
                localClass "verticalSpaceBehind"
            SpaceForward ->
                localClass "verticalSpaceForward"
            SpaceBoth ->
                localClass "verticalSpaceBoth"
        , case layout.horizontalSpace of
            SpaceBehind ->
                localClass "horizontalSpaceBehind"
            SpaceForward ->
                localClass "horizontalSpaceForward"
            SpaceBoth ->
                localClass "horizontalSpaceBoth"
        ]


childClass : String -> View_ msg -> Mixin msg
childClass prefix child =
    case child of
        Top _ -> Mixin.none
        Boundary o ->
            childClass_ prefix o.layout
        Scroller o ->
            childClass_ prefix o.layout
        Row o ->
            childClass_ prefix o.layout
        Column o ->
            childClass_ prefix o.layout
        TextNode o ->
            childClass_ prefix o.layout
        None ->
            Mixin.none

childClass_ : String -> Layout -> Mixin msg
childClass_ prefix layout =
    let
        localClass str = class <| prefix ++ "-" ++ str
    in
    Mixin.batch
        [ case layout.maxWidth of
            MaxWidthInfinite ->
                localClass "maxWidthInfinite"
            MaxWidthFit ->
                localClass "maxWidthFit"
            MaxWidthInBs _ ->
                localClass "maxWidthInUnit"
            MaxWidthInUnit _ _ ->
                localClass "maxWidthInUnit"
        , case layout.maxHeight of
            MaxHeightInfinite ->
                localClass "maxHeightInfinite"
            MaxHeightFit ->
                localClass "maxHeightFit"
            MaxHeightInBs _ ->
                localClass "maxHeightInUnit"
            MaxHeightInUnit _ _ ->
                localClass "maxHeightInUnit"
        ]


class : String -> Mixin msg
class str =
    Mixin.class <| "elmNeatLayout--" ++ str


{-| Dangerous work around. It overwrites `Attribtes.style`.
-}
customProperty : List (String, String) -> Mixin msg
customProperty kvs =
    kvs
        |> List.foldl (\(k, v) acc -> acc ++ k ++ ":" ++ v ++ ";") ""
        |> Attributes.attribute "style"
        |> Mixin.fromAttribute


-- Low level function for HTML


{-| -}
setNodeName : String -> View gap msg -> View gap msg
setNodeName str =
    modifyLayout <|
        \layout ->
            { layout
                | nodeName = str
            }
