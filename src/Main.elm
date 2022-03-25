module Main exposing (Model, Msg, main)

import Browser exposing (Document)
import Gap
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Mixin
import Mixin.Html as Mixin
import Neat exposing (Boundary, View, NoGap)



-- App

main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ()


init : ( Model, Cmd Msg )
init =
    ( ()
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "Sample"
    , body =
        [ body model
            |> Neat.render Neat.defaultRenderer
        ]
    }


body : Model -> Boundary Msg
body _ =
    Neat.column
        ( Neat.defaultColumn
            |> Neat.alignBottom
        )
        [ Neat.row
            Neat.defaultRow
            [ Neat.textBlock "Header"
                |> Neat.setMaxWidthInfinite
                |> Neat.setGap Gap.body
                |> Neat.middleItem "text"
            , Neat.textBlock "三"
                |> Neat.setMinWidthInEm 3
                |> Neat.setMaxWidthInEm 3
                |> Neat.setMinHeightInEm 3
                |> Neat.setMixin (Mixin.class "header_hamburger")
                |> Neat.setGap Gap.body
                |> Neat.middleItem "hamburger"
            ]
                |> Neat.setBoundary
                |> Neat.setMaxWidthInfinite
                |> Neat.setMixin (Mixin.class "header")
                |> Neat.setGap Neat.noGap
                |> Neat.leftItem "header"
        , Neat.column
            Neat.defaultColumn
            [ Neat.textBlock
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                -- |> Neat.setMinHeightInEm 10
                |> Neat.setMaxHeightInEm 25
                -- |> Neat.setMaxWidthInEm 35
                |> Neat.setMaxWidthInfinite
                |> Neat.setMinWidthInEm 16
                |> Neat.setGap Gap.body
                |> Neat.leftItem "sampleText"
            , Neat.empty
                |> Neat.setMinHeightInEm 5
                |> Neat.setMinWidthInEm 10
                |> Neat.setMaxWidthInEm 20
                |> Neat.setMaxHeightInfinite
                |> Neat.setMixin (Mixin.class "red")
                |> Neat.setGap Gap.body
                |> Neat.leftItem "sampleBox"
            , Neat.row
                (Neat.defaultRow
                    |> Neat.alignRight
                )
                [ Neat.empty
                    |> Neat.setMinHeightInEm 5
                    |> Neat.setMaxHeightInfinite
                    |> Neat.setMinWidthInEm 12
                    |> Neat.setMaxWidthInEm 23
                    |> Neat.setMixin (Mixin.class "blue")
                    |> Neat.setGap Gap.body
                    |> Neat.grownBottomItem "sampleBox"
                ]
                |> Neat.setBoundary
                |> Neat.setMixin (Mixin.class "red")
                |> Neat.setMinWidthInEm 37
                |> Neat.setMaxWidthInEm 40
                |> Neat.setMaxHeightInEm 33
                |> Neat.setGap Gap.body
                |> Neat.grownCenterItem "sampleNestedBox"
            ]
            |> Neat.setBoundary
            -- |> Neat.setMaxHeightInfinite
            |> Neat.setMinHeightInEm 10
            |> Neat.setMaxWidthInfinite
            |> Neat.enableVerticalScroll
            |> Neat.setMixin (Mixin.class "blue")
            |> Neat.setGap Neat.noGap
            |> Neat.grownRightItem "body"
        , Neat.textBlock "Footer"
            |> Neat.setGap Gap.body
            |> Neat.setBoundary
            |> Neat.setMaxWidthInfinite
            |> Neat.setMixin (Mixin.class "footer")
            |> Neat.setGap Neat.noGap
            |> Neat.rightItem "footer"
        ]
        |> Neat.setBoundary
        |> Neat.setMaxHeightInfinite
        |> Neat.setMaxWidthInfinite


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
