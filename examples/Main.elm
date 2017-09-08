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

type AB = A | B

init : (Model, Cmd Msg)
init =
    ( { snackbar = Snackbar.empty , state = A }
    , Cmd.none
    )

snackbarConfig : Snackbar.Config msg
snackbarConfig =
    Snackbar.Mdl.inline
        { backgroundColor = Color.black
        , actionColor = Color.yellow
        , fontFamily = Just "sans-serif"
        }

view : Model -> Html Msg
view { snackbar, state } =
    div []
        [ Snackbar.view snackbarConfig snackbar
        , button [ onClick Trigger ]
            [ text <| toString state ]
        ]
        
type Msg
    = UpdateSnackbar Snackbar.Msg
    | Trigger
    | Toggle

snackbarUpdateConfig : Snackbar.UpdateConfig Msg
snackbarUpdateConfig =
    Snackbar.defaultUpdateConfig UpdateSnackbar

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSnackbar submsg ->
            let
                (newSnackbar, subcmd) =
                    Snackbar.update snackbarUpdateConfig submsg model.snackbar
            in
                ( { model | snackbar = newSnackbar }
                , subcmd
                )

        Trigger ->
            let
                (newSnackbar, subcmd) =
                    Snackbar.push 
                        snackbarUpdateConfig 
                        20000
                        { message = "Try clicking here to toggle the button"
                        , action = Toggle
                        , actionText = (toString <| toggle model.state)
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
                |> andTransitionOut

andTransitionOut : (Model, Cmd Msg) -> (Model, Cmd Msg)
andTransitionOut (model, cmd) =
    let
        (newSnackbar, subcmd) =
            Snackbar.update snackbarUpdateConfig Snackbar.TransitionOut  model.snackbar
    in
        ( { model | snackbar = newSnackbar }
        , Cmd.batch [ cmd, subcmd ]
        )


toggle : AB -> AB
toggle state =
   case state of
       A -> B
       B -> A



