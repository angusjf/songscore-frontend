module Styles exposing (..)

import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font

red = E.rgb 1.0 0.4 0.4
white = E.rgb 1.0 1.0 1.0

roundedSmall = Border.rounded 8

paddingSmall = E.paddingXY 8 4
paddingMedium = E.paddingXY 16 8

spacingMedium = E.spacing 16

text str = E.el [] <| E.text str
textAlt str = E.el [ Font.color white, Font.bold ] <| E.text str

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
