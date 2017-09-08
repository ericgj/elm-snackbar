module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Snackbar


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

view : Model -> Html Msg
view { snackbar, state } =
    div []
        [ Snackbar.view Snackbar.emptyConfig snackbar
        , button [ onClick Trigger ]
            [ text <| toString state ]
        ]
        
type Msg
    = UpdateSnackbar Snackbar.Msg
    | Trigger
    | Toggle

snackbarConfig : Snackbar.UpdateConfig Msg
snackbarConfig =
    Snackbar.defaultUpdateConfig UpdateSnackbar

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSnackbar submsg ->
            let
                (newSnackbar, subcmd) =
                    Snackbar.update snackbarConfig submsg model.snackbar
            in
                ( { model | snackbar = newSnackbar }
                , subcmd
                )

        Trigger ->
            let
                (newSnackbar, subcmd) =
                    Snackbar.push 
                        snackbarConfig 3000 
                        { message = "Try clicking this link to toggle the button"
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

toggle : AB -> AB
toggle state =
   case state of
       A -> B
       B -> A



