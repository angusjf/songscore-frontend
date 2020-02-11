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
contentList content = E.column [ E.width E.fill , spacingMedium ] content

--------------------- REVIEWBOX

reviewAndCommentBox :
  { subjectImage : Maybe String
  , subjectArtist : Maybe String
  , subjectTitle : String
  , username : String
  , userImage : Maybe String
  , ownReview : Bool
  , onDelete : Maybe msg
  , onLike : Maybe msg
  , onDislike : Maybe msg
  , reviewStars : Int
  , reviewText : Maybe String
  , comments : List (String, String)
  }
  -> Element msg
reviewAndCommentBox
  { subjectImage, subjectArtist, subjectTitle, username, userImage, ownReview
  , onDelete, onLike, onDislike, reviewStars, reviewText, comments }
  =
    E.column [spacingMedium, lightShadow, paddingMedium]
      [ E.row [spacingMedium]
        [ nStars reviewStars
        ]
      ]
    --      [ E.column [spacingMedium, E.alignTop] 
    --          [ E.image squareMedium
    --              { src = Maybe.withDefault
    --                  "/assets/images/default-subject.png"
    --                  subjectImage
    --              , description = "subject picture"
    --              }
    --          , E.column [] <|
    --              [ text <| Maybe.withDefault "" subjectArtist
    --              , text subjectTitle
    --              ]
    --          ]
    --      , E.column [spacingMedium, E.width (E.px 500), E.alignTop] 
    --          [ E.row [E.width E.fill]
    --              [ E.link []
    --                  { url = "/users/" ++ username
    --                  , label =
    --                      E.row []
    --                        [ E.image circleSmall
    --                            { src =
    --                                Maybe.withDefault
    --                                  "/assets/images/default-user.png"
    --                                  userImage
    --                            , description = "profile picture"
    --                            }
    --                        , text ("@" ++ username)
    --                        ]
    --                  }
    --              ]
    --              
    --                E.row [E.alignRight] <|
    --                  if ownReview
    --                        then [ button "delete" <| onDelete ]
    --                        else []
    --            ]
    --            , E.row [spacingMedium, E.width E.fill]
    --                [ E.column [spacingMedium, E.width E.fill] <|
    --                    [ nStars reviewStars
    --                    , E.paragraph [E.width E.fill] <|
    --                        [ E.text <|
    --                            Maybe.withDefault
    --                              "(this review has no text)" <|
    --                              Maybe.map (\x -> "“" ++ x ++ "”") reviewText
    --                        ]
    --                    ]
    --                , E.row [spacingMedium, E.alignRight, E.alignTop]
    --                    [ button "<3" <| onLike
    --                    , button ":(" <| onDislike
    --                    ]
    --                ]
    --          ]

    --  ,       ]

viewComments : List (String, String) -> Element msg
viewComments comments =
  E.column [E.width E.fill] <|
    List.map comment comments


comment : (String, String) -> Element msg
comment (username, commentText) =
  E.row [spacingMedium, paddingSmall]
    [ E.link []
        { url = "/users/" ++ username
        , label = boldText ("@" ++ username ++ ":")
        }
    , text commentText
    ]
 
nStars : Int -> Element msg
nStars n =
  E.row [spacingSmall] <|
    (List.repeat n redStar) ++ (List.repeat (5 - n) greyStar)

redStar : Element msg
redStar =
  E.image [E.width (E.px 72) , E.height (E.px 72)]
    { src = "/assets/images/red-star.png"
    , description = "red star"
    }

greyStar : Element msg
greyStar =
  E.image [E.width (E.px 72) , E.height (E.px 72)]
    { src = "/assets/images/grey-star.png"
    , description = "grey star"
    }
