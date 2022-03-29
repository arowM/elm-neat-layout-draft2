module Neat exposing
    ( View
    , map
    , NoGap
    , noGap
    , Boundary
    , mapBoundary
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
    , Row
    , defaultRow
    , enableWrap
    , alignCenter
    , alignRight
    , topItem
    , middleItem
    , bottomItem
    , grownTopItem
    , grownMiddleItem
    , grownBottomItem
    , column
    , Column
    , defaultColumn
    , alignMiddle
    , alignBottom
    , leftItem
    , centerItem
    , rightItem
    , grownLeftItem
    , grownCenterItem
    , grownRightItem
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
    , setGap
    , setBoundary
    , enableVerticalScroll
    , enableHorizontalScroll
    , putLayer
    , Layer
    , defaultLayer
    , Layered
    , mapLayered
    , toLayered
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
@docs noGap


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

# Gaps

@docs setGap
@docs setBoundary

# Row

@docs row

## Config

@docs Row
@docs defaultRow
@docs enableWrap
@docs alignCenter
@docs alignRight

## Item

Each function has the `String` argument, which helps make the DOM modifications more efficient. It must be unique among items in the same row.

@docs topItem
@docs middleItem
@docs bottomItem
@docs grownTopItem
@docs grownMiddleItem
@docs grownBottomItem

# Column

@docs column

## Config

@docs Column
@docs defaultColumn
@docs alignMiddle
@docs alignBottom

## Item

Each function has the `String` argument, which helps make the DOM modifications more efficient. It must be unique among items in the same row.

@docs leftItem
@docs centerItem
@docs rightItem
@docs grownLeftItem
@docs grownCenterItem
@docs grownRightItem

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
    = FromBoundary (Boundary_ msg)
    | FromRow (Row_ msg)
    | FromColumn (Column_ msg)
    | None


{-| A bounded View without gap.

Convert to/from View by `setGap`/`setBoundary`.
-}
type Boundary msg
    = Boundary (Boundary_ msg)


type alias Boundary_ msg =
    { mixin : Mixin msg
    , gap : Gap
    , nodeName : String
    , innerGap : Gap
    , overlays : Overlays msg
    , width : Size
    , minWidth : MinWidth
    , maxWidth : MaxWidth
    , horizontalOverflow : Bool
    , height : Size
    , minHeight : MinHeight
    , maxHeight : MaxHeight
    , verticalOverflow : Bool
    , content : Content msg
    , enforcePointerEvent : Bool
    }


type Content msg
    = TextContent (List (Text msg))
    | ViewContent (View_ msg)
    | NoContent


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

type Size
    = MinSize
    | FlexSize


type alias Row_ msg =
    { mixin : Mixin msg
    , gap : Gap
    , nodeName : String
    , justifyContent : Alignment
    , children : Children msg
    , wrap : Bool
    , width : Size
    , height : Size
    }


{-| -}
type Alignment
    = AlignStart
    | AlignCenter
    | AlignEnd


defaultRow_ : Children msg -> Row_ msg
defaultRow_ children =
    { mixin = Mixin.none
    , gap = emptyGap
    , nodeName = "div"
    , justifyContent = AlignStart
    , children = children
    , wrap = True
    , width = FlexSize
    , height = FlexSize
    }



type alias Column_ msg =
    { mixin : Mixin msg
    , gap : Gap
    , nodeName : String
    , justifyContent : Alignment
    , children : Children msg
    , width : Size
    , height : Size
    }


defaultColumn_ : Children msg -> Column_ msg
defaultColumn_ children =
    { mixin = Mixin.none
    , gap = emptyGap
    , nodeName = "div"
    , justifyContent = AlignStart
    , children = children
    , width = FlexSize
    , height = FlexSize
    }


type alias Overlays msg =
    List (Overlay msg)


type alias Overlay msg =
    { name : String
    , area : Layer
    , boundary : Boundary msg
    }


mapOverlays : (a -> b) -> Overlays a -> Overlays b
mapOverlays f =
    List.map
        (\o ->
            { name = o.name
            , area = o.area
            , boundary = mapBoundary f o.boundary
            }
        )


type Children msg
    = Children (Item_ msg) (List (Item_ msg))


{-| -}
type RowItem gap msg =
    RowItem (Item_ msg)


{-| -}
type ColumnItem gap msg =
    ColumnItem (Item_ msg)

type alias Item_ msg =
    { alignSelf : Alignment
    , grow : Bool
    , key : String
    , content : View_ msg
    }


{-| Top-aligned item.
-}
topItem : String -> View gap msg -> RowItem gap msg
topItem key (View content) = RowItem
    { alignSelf = AlignStart
    , grow = False
    , key = key
    , content = content
    }


{-| Top-aligned item which grows its width as much as possible.
-}
grownTopItem : String -> View gap msg -> RowItem gap msg
grownTopItem key (View content) = RowItem
    { alignSelf = AlignStart
    , grow = True
    , key = key
    , content = content
    }


{-| Vertically centered item.
-}
middleItem : String -> View gap msg -> RowItem gap msg
middleItem key (View content) = RowItem
    { alignSelf = AlignCenter
    , grow = False
    , key = key
    , content = content
    }


{-| Vertically centered item which grows its width as much as possible.
-}
grownMiddleItem : String -> View gap msg -> RowItem gap msg
grownMiddleItem key (View content) = RowItem
    { alignSelf = AlignCenter
    , grow = True
    , key = key
    , content = content
    }


{-| Bottom-aligned item.
-}
bottomItem : String -> View gap msg -> RowItem gap msg
bottomItem key (View content) = RowItem
    { alignSelf = AlignEnd
    , grow = False
    , key = key
    , content = content
    }


{-| Bottom-aligned item which grows its width as much as possible.
-}
grownBottomItem : String -> View gap msg -> RowItem gap msg
grownBottomItem key (View content) = RowItem
    { alignSelf = AlignEnd
    , grow = True
    , key = key
    , content = content
    }


{-| Left-aligned item.
-}
leftItem : String -> View gap msg -> ColumnItem gap msg
leftItem key (View content) = ColumnItem
    { alignSelf = AlignStart
    , grow = False
    , key = key
    , content = content
    }


{-| Left-aligned item which grows its height as much as possible.
-}
grownLeftItem : String -> View gap msg -> ColumnItem gap msg
grownLeftItem key (View content) = ColumnItem
    { alignSelf = AlignStart
    , grow = True
    , key = key
    , content = content
    }


{-| Horizontally centered item.
-}
centerItem : String -> View gap msg -> ColumnItem gap msg
centerItem key (View content) = ColumnItem
    { alignSelf = AlignCenter
    , grow = False
    , key = key
    , content = content
    }


{-| Horizontally centered item which grows its height as much as possible.
-}
grownCenterItem : String -> View gap msg -> ColumnItem gap msg
grownCenterItem key (View content) = ColumnItem
    { alignSelf = AlignCenter
    , grow = True
    , key = key
    , content = content
    }


{-| Right-aligned item.
-}
rightItem : String -> View gap msg -> ColumnItem gap msg
rightItem key (View content) = ColumnItem
    { alignSelf = AlignEnd
    , grow = False
    , key = key
    , content = content
    }


{-| Right-aligned item which grows its height as much as possible.
-}
grownRightItem : String -> View gap msg -> ColumnItem gap msg
grownRightItem key (View content) = ColumnItem
    { alignSelf = AlignEnd
    , grow = True
    , key = key
    , content = content
    }

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
map : (a -> b) -> View gap a -> View gap b
map f =
    liftInternal (map_ f)


liftInternal : (View_ a -> View_ b) -> View g1 a -> View g2 b
liftInternal f (View view) =
    View <| f view


map_ : (a -> b) -> View_ a -> View_ b
map_ f view =
    case view of
        FromBoundary boundary ->
            FromBoundary <| mapBoundary_ f boundary

        FromRow o ->
            FromRow
                { mixin = Mixin.map f o.mixin
                , gap = o.gap
                , nodeName = o.nodeName
                , justifyContent = o.justifyContent
                , children = modifyChild (map_ f) o.children
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
                , children = modifyChild (map_ f) o.children
                , height = o.height
                , width = o.width
                }

        None ->
            None


{-| -}
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
    , content = case o.content of
        TextContent texts ->
            TextContent <| List.map (Text.map f) texts
        ViewContent view ->
            ViewContent <| map_ f view
        NoContent ->
            NoContent
    , enforcePointerEvent = o.enforcePointerEvent
    }


extractGap : View_ msg -> Gap
extractGap view =
    case view of
        FromBoundary o -> o.gap
        FromRow o -> o.gap
        FromColumn o -> o.gap
        None -> emptyGap


{-| A primitive type that represents that there is no Gap

For custom gaps, see [Custom gaps](#custom-gaps).

-}
type NoGap
    = NoGap


{-| -}
noGap : IsGap NoGap
noGap =
    IsGap emptyGap


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

The line spacing width when the text is broken is the same as the Gap height applied by the `setGap` function.
-}
fromTexts : List (Text msg) -> Boundary msg
fromTexts ls =
    let
        texts = List.filter (\a -> a.text /= "") ls
    in
    Boundary
        { defaultBoundary
            | content =
                if List.isEmpty texts then
                    NoContent
                else
                    TextContent texts
        }


{-| Generates no HTML nodes.
This is useful for handling elements which only appears under certain conditions.
-}
none : View g a
none =
    View None


{-| -}
type Row
    = Row RowConfig

type alias RowConfig =
    { justify : Alignment
    , wrap : Bool
    }


{-| Default setting for rows.

* horizontal alignment: left
* wrapping: disabled

-}
defaultRow : Row
defaultRow = Row
    { justify = AlignStart
    , wrap = False
    }


{-| -}
alignCenter : Row -> Row
alignCenter (Row config) =
    Row
        { config | justify = AlignCenter }


{-| -}
alignRight : Row -> Row
alignRight (Row config) =
    Row
        { config | justify = AlignEnd }


{-| -}
enableWrap : Row -> Row
enableWrap (Row config) =
    Row
        { config | wrap = True }


{-| -}
type Column
    = Column ColumnConfig

type alias ColumnConfig =
    { justify : Alignment
    }


{-| Default setting for columns.

* vertical alignment: top

-}
defaultColumn : Column
defaultColumn = Column
    { justify = AlignStart
    }


{-| -}
alignMiddle : Column -> Column
alignMiddle (Column config) =
    Column
        { config | justify = AlignCenter }


{-| -}
alignBottom : Column -> Column
alignBottom (Column config) =
    Column
        { config | justify = AlignEnd }



{-| Align children horizontally.
-}
row : Row -> List (RowItem gap msg) -> View gap msg
row (Row { justify, wrap }) children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\(RowItem item) ->
                        if item.content == None then
                            Nothing
                        else
                            Just item
                    )
    in
    View <|
        case children of
            [] ->
                None

            (item :: items) ->
                let
                    row_ = defaultRow_ (Children item items)
                in
                FromRow
                    { row_
                        | gap = extractGap item.content
                        , justifyContent = justify
                        , wrap = wrap
                    }


{-| Align children vertically.
-}
column : Column -> List (ColumnItem gap msg) -> View gap msg
column (Column { justify }) children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\(ColumnItem item) ->
                        if item.content == None then
                            Nothing
                        else
                            Just item
                    )
    in
    View <|
        case children of
            [] ->
                None

            (item :: items) ->
                let
                    column_ = defaultColumn_ <| Children item items
                in
                FromColumn
                    { column_
                        | gap = extractGap item.content
                        , justifyContent = justify
                    }


-- Setter


{-| Append `Mixin` on boundaries.
-}
setMixin : Mixin msg -> Boundary msg -> Boundary msg
setMixin new (Boundary boundary ) =
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
    setMixin <| Mixin.fromAttribute attr


{-| Same as `setAttribute` but takes a list of `Attribute`s.
-}
setAttributes : List (Attribute msg) -> Boundary msg -> Boundary msg
setAttributes attrs =
    setMixin <| Mixin.fromAttributes attrs


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> View g msg -> View g msg
setRole str =
    setViewMixin (Mixin.attribute "role" str)


setViewMixin : Mixin msg -> View g msg -> View g msg
setViewMixin new (View view) =
    View <| case view of
        FromBoundary boundary ->
            FromBoundary { boundary | mixin = Mixin.batch [ boundary.mixin, new ] }
        FromRow row_ ->
            FromRow { row_ | mixin = Mixin.batch [ row_.mixin, new ] }
        FromColumn column_ ->
            FromColumn { column_ | mixin = Mixin.batch [ column_.mixin, new ] }
        None -> None


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> View g msg -> View g msg
setAria name v =
    setViewMixin (Mixin.attribute ("aria-" ++ name) v)


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

minWidthZero : MinWidth -> Bool
minWidthZero minWidth =
    case minWidth of
        MinWidthInBs a -> a == 0
        MinWidthInUnit _ a -> a == 0

{-| -}
type MaxWidth
    = MaxWidthNone
    | MaxWidthFit
    | MaxWidthInBs Float
    | MaxWidthInUnit String Float


{-| -}
type MinHeight
    = MinHeightInBs Float
    | MinHeightInUnit String Float


minHeightZero : MinHeight -> Bool
minHeightZero minHeight =
    case minHeight of
        MinHeightInBs a -> a == 0
        MinHeightInUnit _ a -> a == 0

{-| -}
type MaxHeight
    = MaxHeightNone
    | MaxHeightFit
    | MaxHeightInBs Float
    | MaxHeightInUnit String Float


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


-- Gap


{-| Gap around a view.
-}
setGap : IsGap gap -> Boundary msg -> View gap msg
setGap (IsGap gap) (Boundary boundary) =
    View <| case boundary.content of
        NoContent -> None
        _ ->
            FromBoundary
                    { boundary
                        | gap = gap
                    }



{-| Wrap a view with boundary without gap.
This is the only way to reset view gaps.
-}
setBoundary : View gap msg -> Boundary msg
setBoundary (View view) =
    Boundary
        { defaultBoundary
            | innerGap = extractGap view
            , content =
                if view == None then
                    NoContent
                else
                    ViewContent view
        }


enableVerticalScroll : Boundary msg -> Boundary msg
enableVerticalScroll (Boundary boundary) =
    Boundary
        { boundary
            | verticalOverflow = True
        }


enableHorizontalScroll : Boundary msg -> Boundary msg
enableHorizontalScroll (Boundary boundary) =
    Boundary
        { boundary
            | horizontalOverflow = True
        }



-- Render


{-| Render the `View` into `Html` so that it spreads across the screen.
-}
render : Renderer -> Boundary msg -> Html msg
render (Renderer renderer) (Boundary boundary) =
    let
        childMixin =
            { inherit =
                Mixin.batch
                    [ class "heightFlex"
                    , class "widthFlex"
                    ]
            , self = class "top"
            }
    in
        { boundary
            | height = FlexSize
            , width = FlexSize
        }
            |> preprocessHeight
            |> preprocessWidth
            |> renderBoundary renderer childMixin []


preprocessHeight : Boundary_ msg -> Boundary_ msg
preprocessHeight boundary =
    case boundary.content of
        NoContent -> boundary
        TextContent _ -> boundary
        ViewContent view ->
            let
                helper : Size -> Boundary_ msg
                helper size =
                    { boundary
                        | content = ViewContent <| setHeight size view
                        , height = size
                    }
            in
            case (boundary.verticalOverflow, (boundary.maxHeight, minHeightZero boundary.minHeight), boundary.height) of
                (True, _, _) ->
                    helper FlexSize
                (False, (MaxHeightNone, _), MinSize) ->
                    helper MinSize
                (False, (MaxHeightNone, _), FlexSize) ->
                    helper FlexSize
                (False, (MaxHeightFit, True), _) ->
                    helper MinSize
                (False, (MaxHeightFit, False), _) ->
                    helper FlexSize
                (False, (MaxHeightInBs _, _), _) ->
                    helper FlexSize
                (False, (MaxHeightInUnit _ _, _), _) ->
                    helper FlexSize


setHeight : Size -> View_ msg -> View_ msg
setHeight size view =
    case view of
        FromBoundary boundary ->
            FromBoundary <|
                preprocessHeight
                    { boundary |
                        height = prodSize boundary.height size

                    }

        FromRow row_ ->
            let
                helper s = FromRow
                    { row_
                        | children = modifyChild (setHeight s) row_.children
                        , height = s
                    }
            in
            case size of
                MinSize ->
                    helper FlexSize
                _ ->
                    helper size

        FromColumn column_ ->
            FromColumn
                { column_
                    | children =
                        modifyChild
                            (setHeight size)
                            column_.children
                    , height = size
                }

        None ->
            None


prodSize : Size -> Size -> Size
prodSize sa sb =
    case (sa, sb) of
        (MinSize, _) ->
            MinSize
        (_, MinSize) ->
            MinSize
        (FlexSize, FlexSize) ->
            FlexSize


preprocessWidth : Boundary_ msg -> Boundary_ msg
preprocessWidth boundary =
    case boundary.content of
        NoContent -> boundary
        TextContent _ -> boundary
        ViewContent view ->
            let
                helper : Size -> Boundary_ msg
                helper size =
                    { boundary
                        | content = ViewContent <| setWidth size view
                        , width = prodSize boundary.width size
                    }
            in
            case (boundary.horizontalOverflow, (boundary.maxWidth, minWidthZero boundary.minWidth), boundary.width) of
                (True, _, _) ->
                    helper FlexSize
                (False, (MaxWidthNone, _), MinSize) ->
                    helper MinSize
                (False, (MaxWidthNone, _), FlexSize) ->
                    helper FlexSize
                (False, (MaxWidthFit, True), _) ->
                    helper MinSize
                (False, (MaxWidthFit, False), _) ->
                    helper FlexSize
                (False, (MaxWidthInBs _, _), _) ->
                    helper FlexSize
                (False, (MaxWidthInUnit _ _, _), _) ->
                    helper FlexSize


setWidth : Size -> View_ msg -> View_ msg
setWidth size view =
    case view of
        FromBoundary boundary ->
            FromBoundary <|
                preprocessWidth
                    { boundary |
                        width = size
                    }

        FromRow row_ ->
            FromRow
                { row_
                    | children =
                        modifyChild
                            (setWidth size)
                            row_.children
                    , width = size
                }

        FromColumn column_ ->
            let
                helper s =
                    FromColumn
                        { column_
                            | children =
                                modifyChild
                                    (setWidth s)
                                    column_.children
                            , width = s
                        }
            in
            case size of
                MinSize ->
                    helper FlexSize
                _ ->
                    helper size

        None ->
            None


type alias ChildMixin msg =
    { inherit : Mixin msg
    , self : Mixin msg
    }


render_ : Renderer_
    ->  ChildMixin msg
    -> View_ msg
    -> Html msg
render_ renderer childMixin view =
    case view of
        FromBoundary o ->
            renderBoundary renderer childMixin [] o

        FromRow o ->
            renderRow renderer childMixin o

        FromColumn o ->
            renderColumn renderer childMixin o

        None ->
            Html.text ""


renderBoundary : Renderer_ -> ChildMixin msg -> List (String, String) -> Boundary_ msg -> Html msg
renderBoundary renderer { inherit, self } custom o =
    let
        childMixin =
            { inherit =
                Mixin.batch
                    [ case o.height of
                        MinSize ->
                            class "heightMinSize"
                        FlexSize ->
                            class "heightFlex"
                    , case o.width of
                        MinSize ->
                            class "widthMinSize"
                        FlexSize ->
                            class "widthFlex"
                    ]
            , self = class "boundaryContent"
            }
        base =
            [ boundaryCustomProperty renderer custom o
            , childMixin.inherit
            , self
            , class "boundary"
            , if o.horizontalOverflow then
                class "horizontalOverflow"
              else
                Mixin.none
            , if o.verticalOverflow then
                class "verticalOverflow"
              else
                Mixin.none
            , if o.maxHeight /= MaxHeightNone then
                class "boundary-hasMaxHeight"
              else
                Mixin.none
            , if o.maxWidth /= MaxWidthNone then
                class "boundary-hasMaxWidth"
              else
                Mixin.none
            ]
        core = Mixin.batch
            [ o.mixin
            , class "boundary_core"
            , if not <| List.isEmpty o.overlays then
                class "boundary_core-hasOverlays"
              else
                Mixin.none
            , if o.enforcePointerEvent then
                class "boundary_core-enforcePointerEvent"
              else
                Mixin.none
            ]
    in
    case o.content of
        NoContent ->
            Html.text ""
        ViewContent content ->
            if o.verticalOverflow && o.innerGap.vertical /= 0 then
                Mixin.lift (Html.node o.nodeName)
                    base
                    [ Mixin.div
                        [ core
                        ]
                        [ Mixin.div
                            [ class "boundary_scroller"
                            , class "boundary_scroller-verticalScroll"
                            ]
                            ( render_ renderer childMixin content ::
                                List.map (renderOverlay renderer) o.overlays
                            )
                        ]
                    ]
            else
                Mixin.lift (Html.node o.nodeName)
                    base
                    [ Mixin.div
                        [ core
                        , class "boundary_core-view"
                        ]
                        ( render_ renderer childMixin content ::
                            List.map (renderOverlay renderer) o.overlays
                        )
                    ]
        TextContent texts ->
            Mixin.lift (Html.node o.nodeName)
                base
                [ texts
                    |> List.map
                        (\inline ->
                            Mixin.lift (Html.node inline.nodeName)
                                [ inline.mixin
                                , class "boundary_core_text"
                                ]
                                [ Html.text inline.text
                                ]
                        )
                    |> (\children -> children ++
                            List.map (renderOverlay renderer) o.overlays
                       )

                    |> Mixin.div
                        [ core
                        , class "boundary_core-text"
                        ]
                ]



boundaryCustomProperty : Renderer_ -> List (String, String) -> Boundary_ msg -> Mixin msg
boundaryCustomProperty renderer custom o =
    customProperty <| custom ++
        [ ( "--outer-gap-x"
          , multipleBaseSize o.gap.horizontal renderer.baseSize
                  |> renderBaseSize
          )
        , ( "--outer-gap-y"
          , multipleBaseSize o.gap.vertical renderer.baseSize
                  |> renderBaseSize
          )
        , ( "--inner-gap-x"
          , multipleBaseSize o.innerGap.horizontal renderer.baseSize
                  |> renderBaseSize
          )
        , ( "--inner-gap-y"
          , multipleBaseSize o.innerGap.vertical renderer.baseSize
                  |> renderBaseSize
          )
        ] ++ List.concat
            [ case o.minWidth of
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
            , case o.maxWidth of
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
            , case o.minHeight of
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
            , case o.maxHeight of
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
            ]


renderRow : Renderer_ -> ChildMixin msg -> Row_ msg -> Html msg
renderRow renderer { inherit, self } o =
    let
        base =
            Mixin.batch
                [ o.mixin
                , case o.height of
                    MinSize ->
                        class "heightMinSize"
                    FlexSize ->
                        class "heightFlex"
                , case o.width of
                    MinSize ->
                        class "widthMinSize"
                    FlexSize ->
                        class "widthFlex"
                , self
                , class "row"
                , if o.wrap then
                    class "row-wrap"
                  else
                    Mixin.none
                , case o.justifyContent of
                    AlignStart ->
                        class "row-justifyStart"
                    AlignCenter ->
                        class "row-justifyCenter"
                    AlignEnd ->
                        class "row-justifyEnd"
                ]

        childMixin item =
            { inherit = inherit
            , self = Mixin.batch
                [ class "rowChild"
                , if item.grow then
                    class "rowChild-grow"
                  else
                    Mixin.none
                , case item.alignSelf of
                    AlignStart ->
                        class "rowChild-alignStart"
                    AlignCenter ->
                        class "rowChild-alignCenter"
                    AlignEnd ->
                        class "rowChild-alignEnd"
                ]
            }

    in
    case o.children of
        Children item [] ->
            Keyed.node o.nodeName
                (Mixin.toAttributes <| Mixin.batch
                    [ base
                    , class "row-single"
                    ]
                )
                [ (item.key, render_ renderer (childMixin item) item.content)
                ]

        Children item0 items ->
            (item0 :: items)
                |> List.map
                    (\item ->
                        (item.key, render_ renderer (childMixin item) item.content)
                    )
                |> Keyed.node o.nodeName
                    (Mixin.toAttributes <| Mixin.batch
                        [ base
                        , class "row-multi"
                        ]
                    )


renderColumn : Renderer_ -> ChildMixin msg -> Column_ msg -> Html msg
renderColumn renderer { inherit, self } o =
    let
        base =
            Mixin.batch
                [ o.mixin
                , case o.height of
                    MinSize ->
                        class "heightMinSize"
                    FlexSize ->
                        class "heightFlex"
                , case o.width of
                    MinSize ->
                        class "widthMinSize"
                    FlexSize ->
                        class "widthFlex"
                , self
                , class "column"
                , case o.justifyContent of
                    AlignStart ->
                        class "column-justifyStart"
                    AlignCenter ->
                        class "column-justifyCenter"
                    AlignEnd ->
                        class "column-justifyEnd"
                ]

        childMixin item =
            { inherit = inherit
            , self = Mixin.batch
                [ class "columnChild"
                , if item.grow then
                    class "columnChild-grow"
                  else
                    Mixin.none
                , case item.alignSelf of
                    AlignStart ->
                        class "columnChild-alignStart"
                    AlignCenter ->
                        class "columnChild-alignCenter"
                    AlignEnd ->
                        class "columnChild-alignEnd"
                ]
            }

    in
    case o.children of
        Children item [] ->
            Keyed.node o.nodeName
                (Mixin.toAttributes <| Mixin.batch
                    [ base
                    , class "column-single"
                    ]
                )
                [ (item.key, render_ renderer (childMixin item) item.content)
                ]

        Children item0 items ->
            (item0 :: items)
                |> List.map
                    (\item ->
                        (item.key, render_ renderer (childMixin item) item.content)
                    )
                |> Keyed.node o.nodeName
                    (Mixin.toAttributes <| Mixin.batch
                        [ base
                        , class "column-multi"
                        ]
                    )



renderOverlay : Renderer_ -> Overlay msg -> Html msg
renderOverlay renderer overlay =
    let
        (Boundary boundary) = overlay.boundary
        childMixin =
            { inherit =
                Mixin.batch
                    [ class "heightFlex"
                    , class "widthFlex"
                    ]
            , self = class "overlay"
            }
    in
        { boundary
            | height = FlexSize
            , width = FlexSize
        }
            |> preprocessHeight
            |> preprocessWidth
            |> renderBoundary renderer childMixin
                [ ( "--overlay-top", String.fromFloat overlay.area.top ++ "%")
                , ( "--overlay-bottom", String.fromFloat overlay.area.bottom ++ "%")
                , ( "--overlay-left", String.fromFloat overlay.area.left ++ "%")
                , ( "--overlay-right", String.fromFloat overlay.area.right ++ "%")
                , ( "--overlay-priority"
                  , overlay.area.priority
                    |> Maybe.map String.fromInt
                    |> Maybe.withDefault "auto"
                  )
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
setNodeName str (View view) =
    View <| case view of
        FromBoundary boundary ->
            FromBoundary { boundary | nodeName = str }
        FromRow row_ ->
            FromRow { row_ | nodeName = str }
        FromColumn column_ ->
            FromColumn { column_ | nodeName = str }
        None -> None


-- Layer


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



{-| Put overlay layer on the parent view.
-}
putLayer : String -> ( Layer, Boundary (Layered msg) ) -> Boundary msg -> Boundary msg
putLayer name ( area, layered ) (Boundary boundary) =
    Boundary
        { boundary
            | overlays =
                { name = name
                , area = area
                , boundary = mapBoundary (\(Layered a) -> a) layered
                }
                :: boundary.overlays
        }


{-| -}
type Layered msg
    = Layered msg


{-| -}
mapLayered : (a -> b) -> Boundary (Layered a) -> Boundary (Layered b)
mapLayered f =
    mapBoundary (\(Layered a) -> Layered <| f a)


{-| Convert `Boundary` for `putLayer`. The `Boundary (Layered msg)` ignores pointer events; this feature is especially helpfull for realizing popups with clickable background.

-}
toLayered : Boundary msg -> Boundary (Layered msg)
toLayered (Boundary boundary) =
    Boundary
        { boundary | enforcePointerEvent = True
        }
            |> mapBoundary Layered
