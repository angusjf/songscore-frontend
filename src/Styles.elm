module Styles exposing (..)

import Element as E exposing (Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Html

red = E.rgb 1.0 0.4 0.4
white = E.rgb 1.0 1.0 1.0
veryLightAlpha = E.rgba 0 0 0 0.1

roundedSmall = Border.rounded 8

paddingMixedSmall = E.paddingXY 8 4
paddingMixedMedium = E.paddingXY 16 8

paddingSmall = E.paddingXY 8 8
paddingMedium = E.paddingXY 16 16
spacingSmall = E.spacing 4
spacingMedium = E.spacing 16

text str = E.el [ Font.size 16 ] <| E.text str
textAlt str = E.el [ Font.size 16, Font.color white, Font.bold ] <| E.text str
boldText str = E.el [ Font.size 16, Font.bold ] <| E.text str

button str action =
  Input.button
    [ Background.color red
    , Font.color white
    , Font.bold
    , paddingSmall
    , roundedSmall
    ]
    { onPress = action
    , label = text str
    }

buttonAlt str action =
  Input.button
    [ Background.color white
    , Font.color red
    , Font.bold
    , paddingSmall
    , roundedSmall
    ]
    { onPress = action
    , label = text str
    }

labelSmall str =
  Input.labelAbove [] (textSmall str)

textSmall str = E.el [ Font.size 16 ] (E.text str)

borderSmall = Border.width 3

squareMedium =
--  [ E.width (E.maximum 200 E.fill)
--  , E.height (E.maximum 200 E.fill)
--  ]
  [ E.width (px 140)
  , E.height (px 140)
  ]

circleSmall = 
  [ E.width (px 70)
  , E.height (px 70)
  , Border.rounded 140
  ]

lightShadow =
  Border.shadow
    { offset = (0, 0)
    , size = 0
    , blur = 15
    , color = veryLightAlpha
    }

{-
onEnter : msg -> Element.Attribute msg
onEnter msg =
  let 
   decoder = D.string |> D.andThen checkEnter
   checkEnter key = if key == "Enter"
                      then D.succeed msg
                      else D.fail "Not the enter key"
  in
    Element.htmlAttribute <|
      Html.Events.on "keyup" <|
        D.field "key" decoder
        -}

----- now for something different

skeleton : Element msg -> Element msg -> Html.Html msg
skeleton bar body =
  E.layout [ E.width E.fill ] <|
    E.column
      [ E.width (E.maximum 1000 E.fill)
      , E.centerX
      ]
      [ bar
      , E.el [ E.padding 8 ] body
      ]

loading : Maybe a -> (a -> Element msg) -> Element msg
loading maybeA showA =
  case maybeA of
    Just a -> showA a
    Nothing -> text "Loading..."

page : List (Element msg) -> Element msg
page contents =
  E.column [ E.width E.fill, spacingMedium ] contents

userProfile : String -> Maybe String -> Element msg
userProfile username maybeImage =
  E.row
    [ E.width E.fill
    , spacingMedium
    ] <|
    case maybeImage of
      Just image ->
        [ E.image squareMedium
          { src = image
          , description = "profile picture"
          }
        , text username
        ]
      Nothing -> [ text username ]

contentList : List (Element msg) -> Element msg
contentList content = E.row [ E.width E.fill , spacingMedium ] content

