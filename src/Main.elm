module Main exposing (Model, Msg, main)

import Browser exposing (Document)
import Gap
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Mixin
import Mixin.Html as Mixin
import Neat exposing (View, NoGap)



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


body : Model -> View NoGap Msg
body _ =
    Neat.column
        [ Neat.nowrapRow
            [ Neat.textBlock Gap.body "Header"
            , Neat.textBlock Gap.body "三"
                -- |> Neat.setMaxWidthFit
                |> Neat.setBoundary
                |> Neat.setMinWidthInEm 3
                |> Neat.setMaxWidthInEm 3
                |> Neat.setMinHeightInEm 3
                |> Neat.pullChildrenVCenter
                |> Neat.pullChildrenHCenter
                |> Neat.setMixin (Mixin.class "header_hamburger")
                |> Neat.setGap Gap.body
            ]
                |> Neat.setBoundary
                |> Neat.setMaxHeightFit
                |> Neat.pullChildrenVCenter
                |> Neat.setMixin (Mixin.class "header")
        , Neat.column
            [ Neat.textBlock Gap.body
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                -- |> Neat.setMinHeightInEm 10
                -- |> Neat.setMaxHeightInEm 25
                -- |> Neat.setMaxWidthInEm 25
                -- |> Neat.setMinWidthInEm 16
            , Neat.empty
                |> Neat.setMinHeightInEm 5
                |> Neat.setMinWidthInEm 10
                |> Neat.setMaxWidthInEm 20
                |> Neat.setMixin (Mixin.class "red")
                |> Neat.setGap Gap.body
            , Neat.empty
                |> Neat.setMinHeightInEm 5
                |> Neat.setMinWidthInEm 12
                |> Neat.setMixin (Mixin.class "blue")
                |> Neat.setGap Gap.body
                |> Neat.setBoundary
                |> Neat.pullChildrenRight
                |> Neat.setMixin (Mixin.class "red")
                |> Neat.setMinWidthInEm 7
                |> Neat.setMaxWidthFit
                |> Neat.setMaxHeightInEm 3
                |> Neat.setGap Gap.body
            ]
                |> Neat.setBoundary
                |> Neat.setMinHeightInEm 0
        , Neat.textBlock Gap.body "Footer"
            |> Neat.setBoundary
            |> Neat.setMaxHeightFit
            |> Neat.setMixin (Mixin.class "footer")
        ]
        |> Neat.pullChildrenHCenter
        |> Neat.pullChildrenBottom
        |> Neat.setBoundary
        |> Neat.setMinWidthInEm 0


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
