module Snackbar.Mdl
    exposing
        ( Config
        , Position(..)
        , inline
        , classes
        )

import Color exposing (Color)
import Html
import Html.Attributes exposing (class, style)
import Snackbar


type alias Config msg =
    { updateMsg : Snackbar.Msg msg -> msg
    , backgroundColor : Color
    , actionColor : Color
    , fontFamily : Maybe String
    , position : Position
    }


type Position
    = TopLeft String String
    | TopRight String String
    | BottomLeft String String
    | BottomRight String String


inline : Config msg -> Snackbar.Config msg
inline { updateMsg, backgroundColor, actionColor, fontFamily, position } =
    Snackbar.emptyConfig updateMsg
        |> Snackbar.setDelayTransitioningIn 0
        |> Snackbar.setDelayTransitioningOut 2000
        |> Snackbar.setAttributes (containerStyle position backgroundColor fontFamily)
        |> Snackbar.setActiveAttributes activeStyle
        |> Snackbar.setMessageAttributes messageStyle
        |> Snackbar.setActionAttributes (actionStyle actionColor)
        |> Snackbar.setTransitioningInAttributes transitioningInStyle
        |> Snackbar.setTransitioningOutAttributes transitioningOutStyle


classes : Config msg -> Snackbar.Config msg
classes { updateMsg, backgroundColor, actionColor, fontFamily, position } =
    Snackbar.emptyConfig updateMsg
        |> Snackbar.setDelayTransitioningIn 0
        |> Snackbar.setDelayTransitioningOut 2000
        |> Snackbar.setAttributes (containerClass position backgroundColor fontFamily)
        |> Snackbar.setActiveAttributes activeClass
        |> Snackbar.setMessageAttributes messageClass
        |> Snackbar.setActionAttributes (actionClass actionColor)


containerClass : Position -> Color -> Maybe String -> List (Html.Attribute msg)
containerClass position color font =
    [ class "mdl-snackbar"
    , style
        ([ ( "background-color", colorToString color )
         ]
            ++ (positionToStyles position)
            ++ (font
                    |> Maybe.map (\f -> [ ( "font-family", f ) ])
                    |> Maybe.withDefault []
               )
        )
    ]


containerStyle : Position -> Color -> Maybe String -> List (Html.Attribute msg)
containerStyle position color font =
    [ style
        ([ ( "position", "fixed" )
         , ( "cursor", "default" )
         , ( "background-color", colorToString color )
         , ( "z-index", "3" )
         , ( "display", "flex" )
         , ( "justify-content", "space-between" )
         , ( "will-change", "transform" )
         , ( "transition", "transform 0.25s cubic-bezier(0.4, 0, 1, 1)" )
         , ( "pointer-events", "none" )
         ]
            ++ (positionToStyles position)
            ++ (font
                    |> Maybe.map (\f -> [ ( "font-family", f ) ])
                    |> Maybe.withDefault []
               )
        )
    ]


activeClass : List (Html.Attribute msg)
activeClass =
    [ class "mdl-snackbar--active" ]


activeStyle : List (Html.Attribute msg)
activeStyle =
    [ style
        [ ( "transform", "translate(0,0)" )
        , ( "pointer-events", "auto" )
        , ( "transition", "transform 0.25s cubic-bezier(0.4, 0, 1, 1)" )
        ]
    ]


messageClass : List (Html.Attribute msg)
messageClass =
    [ class "mdl-snackbar__text" ]


messageStyle : List (Html.Attribute msg)
messageStyle =
    [ style
        [ ( "padding", "14px 12px 14px 24px" )
        , ( "vertical-align", "middle" )
        , ( "color", "white" )
        , ( "float", "left" )
        ]
    ]


actionClass : Color -> List (Html.Attribute msg)
actionClass color =
    [ class "mdl-snackbar__action"
    , style
        [ ( "color", colorToString color )
        ]
    ]


actionStyle : Color -> List (Html.Attribute msg)
actionStyle color =
    [ style
        [ ( "background", "transparent" )
        , ( "border", "none" )
        , ( "color", colorToString color )
        , ( "float", "right" )
        , ( "text-transform", "uppercase" )
        , ( "padding", "14px 24px 14px 12px" )
        , ( "overflow", "hidden" )
        , ( "outline", "none" )
        , ( "cursor", "pointer" )
        , ( "text-decoration", "none" )
        , ( "text-align", "center" )
        , ( "align-self", "center" )
        , ( "font-size", "14px" )
        , ( "font-weight", "500" )
        , ( "line-height", "1" )
        , ( "letter-spacing", "0" )
        ]
    ]


transitioningInStyle : List (Html.Attribute msg)
transitioningInStyle =
    [ style
        [ ( "transition", "transform 0.25s cubic-bezier(0.4, 0, 1, 1)" )
        ]
    ]


transitioningOutStyle : List (Html.Attribute msg)
transitioningOutStyle =
    [ style
        [ ( "transition", "transform 0.25s cubic-bezier(0.4, 0, 1, 1)" )
        ]
    ]


positionToStyles : Position -> List ( String, String )
positionToStyles position =
    case position of
        TopLeft top left ->
            [ ( "top", top )
            , ( "left", left )
            , ( "transform", "translate(0, -100px)" )
            ]

        TopRight top right ->
            [ ( "top", top )
            , ( "right", right )
            , ( "transform", "translate(0, -100px)" )
            ]

        BottomLeft bottom left ->
            [ ( "bottom", bottom )
            , ( "left", left )
            , ( "transform", "translate(0, 100px)" )
            ]

        BottomRight bottom right ->
            [ ( "bottom", bottom )
            , ( "right", right )
            , ( "transform", "translate(0, 100px)" )
            ]


colorToString : Color -> String
colorToString color =
    Color.toRgb color
        |> (\{ red, green, blue, alpha } ->
                "rgba("
                    ++ (toString red)
                    ++ ","
                    ++ (toString green)
                    ++ ","
                    ++ (toString blue)
                    ++ ","
                    ++ (toString alpha)
                    ++ ")"
           )
