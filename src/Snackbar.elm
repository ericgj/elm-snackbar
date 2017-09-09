module Snackbar exposing
    ( Model
    , Notification
    , Msg
    , Config
    , empty
    , emptyConfig
    , setDelayTransitioningIn
    , setDelayTransitioningOut
    , setAttributes
    , setMessageAttributes
    , setActionAttributes
    , setTransitioningInAttributes
    , setActiveAttributes
    , setTransitioningOutAttributes
    , push
    , update
    , view
    )

import Time
import Task exposing (Task)
import Process
import Html exposing (Html)
import Html.Events

-- STACK

type Stack a 
    = Empty
    | TransitioningIn (a, Float) (List (a, Float))
    | Active (a, Float) (List (a, Float))
    | TransitioningOut (a, Float) (List (a, Float))

    
empty : Stack a
empty = 
    Empty
    
    
type Msg msg
    = Activate
    | TransitionOut
    | Pop
    | UserClick msg


update : Config msg -> Msg msg -> Stack a -> ( Stack a, Cmd msg, Maybe msg )
update (Config c) msg model =
    let
        wrap ext tagger (m,cmd) =
            (m, Cmd.map tagger cmd, ext)
    in
        case msg of
            Activate ->
                activate model 
                    |> wrap Nothing c.updateMsg
            
            TransitionOut ->
                transitionOut c.delayTransitioningOut model 
                    |> wrap Nothing c.updateMsg
            
            Pop ->
                pop c.delayTransitioningIn model 
                    |> wrap Nothing c.updateMsg

            UserClick extmsg ->
                transitionOut c.delayTransitioningOut model
                    |> wrap (Just extmsg) c.updateMsg


push : Config msg -> Float -> a -> Stack a -> (Stack a, Cmd msg)    
push (Config c) timeout a model =
    case model of
        Empty ->
            ( TransitioningIn (a, timeout) []
            , delay Activate c.delayTransitioningIn |> Cmd.map c.updateMsg
            )
            
        TransitioningIn current others ->
            ( TransitioningIn current ((a, timeout) :: others)
            , Cmd.none
            )    
        
        Active current others ->
            ( Active current ((a, timeout) :: others)
            , Cmd.none
            )
            
        TransitioningOut current others ->
            ( TransitioningOut current ((a, timeout) :: others)
            , Cmd.none
            )    
            

activate : Stack a -> (Stack a, Cmd (Msg msg))
activate model =
    case model of
        TransitioningIn (current, timeout) others ->
            ( Active (current, timeout) others
            , delay TransitionOut timeout
            )
            
        _ ->
            ( model
            , Cmd.none
            )
            
transitionOut : Float -> Stack a -> (Stack a, Cmd (Msg msg))
transitionOut timeout model =
    case model of
        Active current others ->
            ( TransitioningOut current others
            , delay Pop timeout
            )
            
        _ ->
            ( model
            , Cmd.none
            )
            
pop : Float -> Stack a -> ( Stack a, Cmd (Msg msg) )
pop timeout model =
    case model of        
        TransitioningOut _ (next :: others) ->
            ( TransitioningIn next others
            , delay Activate timeout
            )
            
        TransitioningOut _ [] ->
            ( Empty
            , Cmd.none
            )
            
        _ ->
            ( model
            , Cmd.none
            )


-- CONFIG

{- TODO: really this should be

type Config display msg =
    Config
        { updateMsg : Msg msg -> msg
        , delayTransitioningIn : Float
        , delayTransitioningOut : Float
        , display : display
        }

where for Notifications, 

type alias DisplayConfig msg =
    { attributes : List (Html.Attribute msg)
    , messageAttributes : List (Html.Attribute msg)
    , actionAttributes : List (Html.Attribute msg)
    , transitioningInAttributes : List (Html.Attribute msg)
    , activeAttributes : List (Html.Attribute msg)
    , transitioningOutAttributes : List (Html.Attribute msg)
    }

and

config : Config (DisplayConfig msg) msg

-}

type Config msg =
    Config
        { updateMsg : Msg msg -> msg
        , delayTransitioningIn : Float
        , delayTransitioningOut : Float
        , attributes : List (Html.Attribute msg)
        , messageAttributes : List (Html.Attribute msg)
        , actionAttributes : List (Html.Attribute msg)
        , transitioningInAttributes : List (Html.Attribute msg)
        , activeAttributes : List (Html.Attribute msg)
        , transitioningOutAttributes : List (Html.Attribute msg)
        }

emptyConfig : (Msg msg -> msg) -> Config msg
emptyConfig tagger =
    Config
        { updateMsg = tagger
        , delayTransitioningIn = 0
        , delayTransitioningOut = 0
        , attributes = []
        , messageAttributes = []
        , actionAttributes = []
        , transitioningInAttributes = []
        , activeAttributes = []
        , transitioningOutAttributes = []
        }

setDelayTransitioningIn : Float -> Config msg -> Config msg
setDelayTransitioningIn n (Config c) =
    Config { c | delayTransitioningIn = n }

setDelayTransitioningOut : Float -> Config msg -> Config msg
setDelayTransitioningOut n (Config c) =
    Config { c | delayTransitioningOut = n }

setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes attrs (Config c) =
    Config { c | attributes = attrs }

setMessageAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setMessageAttributes attrs (Config c) =
    Config { c | messageAttributes = attrs }

setActionAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setActionAttributes attrs (Config c) =
    Config { c | actionAttributes = attrs }

setTransitioningInAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setTransitioningInAttributes attrs (Config c) =
    Config { c | transitioningInAttributes = attrs }

setActiveAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setActiveAttributes attrs (Config c) =
    Config { c | activeAttributes = attrs }

setTransitioningOutAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setTransitioningOutAttributes attrs (Config c) =
    Config { c | transitioningOutAttributes = attrs }


            
-- NOTIFICATION
     
type alias Model msg = 
    Stack (Notification msg)

type alias Notification msg =
    { message : String
    , action : msg
    , actionText : String
    }
    

view : Config msg -> Model msg -> Html msg
view (Config c) model =
    case model of
        Empty ->
            Html.text ""
            
        TransitioningIn (notif, _) _ ->
            viewNotification 
                c.updateMsg
                { container = c.attributes ++ c.transitioningInAttributes
                , message = c.messageAttributes
                , action = c.actionAttributes
                }                
                notif
            
        Active (notif, _) _ ->
            viewNotification 
                c.updateMsg
                { container = c.attributes ++ c.activeAttributes
                , message = c.messageAttributes
                , action = c.actionAttributes
                }                
                notif
            
        TransitioningOut (notif, _) _ ->
            viewNotification
                c.updateMsg
                { container = c.attributes ++ c.transitioningOutAttributes
                , message = c.messageAttributes
                , action = c.actionAttributes
                }
                notif

                
viewNotification : 
    (Msg msg -> msg)
    -> { container : List (Html.Attribute msg) 
       , message : List (Html.Attribute msg)
       , action : List (Html.Attribute msg)
       }
    -> Notification msg 
    -> Html msg
viewNotification tagger attribs { message, action, actionText } =
    Html.div attribs.container
        [ Html.div attribs.message [ Html.text message ]
        , Html.button 
            ( (Html.Events.onClick ( UserClick action |> tagger )) :: attribs.action )
            [ Html.text actionText ]
        ]


-- UTILS
         
delay : msg -> Float -> Cmd msg
delay msg timeout =
    Process.sleep timeout
        |> Task.perform (\_ -> msg)

