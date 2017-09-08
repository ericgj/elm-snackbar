module Snackbar.Mdl exposing 
    ( Config
    , inline
    , classes
    )

import Color exposing (Color)
import Html
import Html.Attributes exposing (class, style)
import Snackbar

type alias Config =
    { backgroundColor : Color
    , actionColor : Color
    , fontFamily : Maybe String
    }

inline : Config -> Snackbar.Config msg
inline { backgroundColor, actionColor, fontFamily } =
    Snackbar.emptyConfig
        |> Snackbar.setAttributes (containerStyle backgroundColor fontFamily)
        |> Snackbar.setActiveAttributes activeStyle
        |> Snackbar.setMessageAttributes messageStyle
        |> Snackbar.setActionAttributes (actionStyle actionColor)
        |> Snackbar.setTransitioningInAttributes transitioningInStyle
        |> Snackbar.setTransitioningOutAttributes transitioningOutStyle

classes : Config -> Snackbar.Config msg
classes { backgroundColor, actionColor, fontFamily } =
    Snackbar.emptyConfig
        |> Snackbar.setAttributes (containerClass backgroundColor fontFamily)
        |> Snackbar.setActiveAttributes activeClass
        |> Snackbar.setMessageAttributes messageClass
        |> Snackbar.setActionAttributes (actionClass actionColor)


containerClass : Color -> Maybe String -> List (Html.Attribute msg)
containerClass color font =
    [ class "mdl-snackbar"
    , style
        ( [ ("background-color", colorToString color) 
          ] ++
          ( font
              |> Maybe.map (\f -> [("font-family", f)])
              |> Maybe.withDefault []
          )
        )
    ]

containerStyle : Color -> Maybe String -> List (Html.Attribute msg)
containerStyle color font =
    [ style
        ( [ ("position", "fixed")
          , ("bottom", "0")
          , ("left", "50%")
          , ("cursor", "default")
          , ("background-color", colorToString color)
          , ("z-index", "3")
          , ("display", "block")
          , ("display", "flex")
          , ("justify-content", "space-between")
          , ("will-change", "transform")
          , ("transform", "translate(0, 80px)")
          , ("transition", "transform 0.25s cubic-bezier(0.4, 0, 1, 1)")
          , ("pointer-events", "none")
          ] ++ 
          ( font
              |> Maybe.map (\f -> [("font-family", f)])
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
        [ ("transform", "translate(0,0)")
        , ("pointer-events", "auto")
        , ("transition", "transform 0.25s cubic-bezier(0.4, 0, 1, 1)")
        ]
    ]

messageClass : List (Html.Attribute msg)
messageClass =
    [ class "mdl-snackbar__text" ]

messageStyle : List (Html.Attribute msg)
messageStyle =
    [ style
        [ ("padding", "14px 12px 14px 24px")
        , ("vertical-align", "middle")
        , ("color", "white")
        , ("float", "left")
        ]
    ]

actionClass : Color -> List (Html.Attribute msg)
actionClass color =
    [ class "mdl-snackbar__action"
    , style
        [ ("color", colorToString color)
        ]
    ]

actionStyle : Color -> List (Html.Attribute msg)
actionStyle color =
    [ style
        [ ("background", "transparent")
        , ("border", "none")
        , ("color", colorToString color)
        , ("float", "right")
        , ("text-transform", "uppercase")
        , ("padding", "14px 24px 14px 12px")
        , ("overflow", "hidden")
        , ("outline", "none")
        -- , ("opacity", "0")
        -- , ("pointer-events", "none")
        , ("cursor", "pointer")
        , ("text-decoration", "none")
        , ("text-align", "center")
        , ("align-self", "center")
        , ("font-size", "14px")
        , ("font-weight", "500")
        , ("line-height", "1")
        , ("letter-spacing", "0")
        ]
    ]

transitioningInStyle : List (Html.Attribute msg)
transitioningInStyle =
    [ style
        [ ("transition", "transform 0.25s cubic-bezier(0.4, 0, 1, 1)")
        ]
    ]

transitioningOutStyle : List (Html.Attribute msg)
transitioningOutStyle =
    [ style
        [ ("transition", "transform 0.25s cubic-bezier(0.4, 0, 1, 1)")
        ]
    ]

colorToString : Color -> String
colorToString color =
    Color.toRgb color
        |> (\{ red, green, blue, alpha } -> 
                "rgba(" 
                ++ (toString red) 
                ++ "," ++ (toString green) 
                ++ "," ++ (toString blue) 
                ++ "," ++ (toString alpha) 
                ++ ")"
           )

