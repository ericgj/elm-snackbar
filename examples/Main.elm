module Main exposing (main)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Snackbar
import Snackbar.Mdl


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    { snackbar : Snackbar.Model Msg
    , state : AB
    }


type AB
    = A
    | B


init : ( Model, Cmd Msg )
init =
    ( { snackbar = Snackbar.empty, state = A }
    , Cmd.none
    )


snackbarConfig : Snackbar.Config Msg
snackbarConfig =
    Snackbar.Mdl.inline
        { updateMsg = UpdateSnackbar
        , backgroundColor = Color.black
        , actionColor = Color.yellow
        , fontFamily = Just "sans-serif"
        , position = Snackbar.Mdl.BottomCenter "3rem" "40%"
        }


view : Model -> Html Msg
view { snackbar, state } =
    div []
        [ Snackbar.view snackbarConfig snackbar
        , button [ onClick Trigger ]
            [ text <| toString state ]
        ]


type Msg
    = UpdateSnackbar (Snackbar.Msg Msg)
    | Trigger
    | Toggle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSnackbar submsg ->
            let
                ( newSnackbar, subcmd, extmsg ) =
                    Snackbar.update snackbarConfig submsg model.snackbar
            in
                ( { model | snackbar = newSnackbar }
                , subcmd
                )
                    |> Snackbar.andIfUserAction update extmsg

        Trigger ->
            let
                ( newSnackbar, subcmd ) =
                    Snackbar.push
                        snackbarConfig
                        20000
                        { message = "Try clicking here to toggle the button"
                        , action = Toggle
                        , actionText = "Toggle"
                        }
                        model.snackbar
            in
                ( { model | snackbar = newSnackbar }
                , subcmd
                )

        Toggle ->
            ( { model | state = toggle model.state }
            , Cmd.none
            )


toggle : AB -> AB
toggle state =
    case state of
        A ->
            B

        B ->
            A
