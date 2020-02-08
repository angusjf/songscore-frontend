module Review exposing (..)

import Json.Decode as D
import Json.Encode as E
import Subject exposing (Subject)
import User exposing (User)

import Element exposing (Element, el, text, image, row, column, spacing, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

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
  row []
    [ column []
        [ image []
            { src = Maybe.withDefault "/assets/images/default-user.png" review.user.image
            , description = "profile picture"
            }
        , el [] <| Element.text ("@" ++ review.user.username)
        ]
    , column []
        [ image []
            { src = Maybe.withDefault "/assets/images/default-subject.png" review.subject.image
            , description = "subject picture"
            }
        , el [] <| Element.text review.subject.title
        ]
    , column []
        [ el [] <| Element.text (String.fromInt review.stars ++ "/5")
        , el [] <| Element.text <|
          Maybe.withDefault "(this review has no text)" review.text
        ]
    ]
