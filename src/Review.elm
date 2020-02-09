module Review exposing (..)

import Json.Decode as D
import Json.Encode as E
import Subject exposing (Subject)
import User exposing (User)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Styles as S
import Browser

type alias Review =
  { id : Maybe Int
  , text : Maybe String
  , stars : Int
  , user : User
  , subject : Subject
  }

decoder : D.Decoder Review
decoder =
  D.map5 Review
    (D.maybe (D.field "id" D.int))
    (D.maybe (D.field "text" D.string))
    (D.field "stars" D.int)
    (D.field "user" User.decoder)
    (D.field "subject" Subject.decoder)

encode : Review -> E.Value
encode review =
  let
    id =
      case review.id of
        Just x -> E.int x
        Nothing -> E.null
    text =
      case review.text of
        Just x -> E.string x
        Nothing -> E.null
  in
    E.object
      [ ("id", id)
      , ("text", text)
      , ("stars", E.int review.stars)
      , ("user", User.encode review.user)
      , ("subject", Subject.encode review.subject)
      ]

view : Review -> Element msg
view review =
  E.row [S.spacingMedium, S.lightShadow, S.paddingMedium]
    [ E.column [S.spacingMedium] 
        [ E.image S.squareMedium
            { src = Maybe.withDefault "/assets/images/default-subject.png" review.subject.image
            , description = "subject picture"
            }
        , E.column [] <|
            [ S.text <| Maybe.withDefault "" review.subject.artist
            , S.text review.subject.title
            ]
        ]
    , E.column [S.spacingMedium] 
        [ E.link []
          { url = "/users/" ++ review.user.username
          , label =
              E.row []
                [ E.image S.circleSmall
                    { src = Maybe.withDefault "/assets/images/default-user.png" review.user.image
                    , description = "profile picture"
                    }
                , S.text ("@" ++ review.user.username)
                ]
          }
        , E.column [S.spacingMedium]
            [ viewNStars review.stars
            , E.el [] <| S.text <|
                 Maybe.withDefault "(this review has no text)" review.text
            ]
        ]
    ]
 
viewNStars : Int -> Element msg
viewNStars n =
  E.row [S.spacingSmall] <|
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

main =
  Browser.sandbox
    { init =
      { id = Nothing
      , text = Just "cool album"
      , stars = 1
      , user =
          { id = 0
          , image = Nothing
          , username = "angus"
          }
      , subject =
          { id = Nothing
          , image = Nothing
          , kind = Nothing
          , title = "Song name!"
          , artist = Nothing
          }
      }
    , view = \model -> E.layout [S.paddingMedium] <| view model
    , update = \msg model -> model
    }
