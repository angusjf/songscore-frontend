module Review exposing (..)

import Json.Decode as D
import Json.Encode as E
import Subject exposing (..)
import User exposing (..)

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

reviewDecoder : D.Decoder Review
reviewDecoder =
  D.map5 Review
    (D.field "id" (D.maybe D.int))
    (D.field "text" (D.maybe D.string))
    (D.field "stars" D.int)
    (D.field "user" userDecoder)
    (D.field "subject" subjectDecoder)

encodeReview : Review -> E.Value
encodeReview review =
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
      , ("user", encodeUser review.user)
      , ("subject", encodeSubject review.subject)
      ]

viewReview : Review -> Element msg
viewReview review =
  column []
    [ row []
        [ el [] <| Element.text ("@" ++ review.user.username)
        , image []
            { src = Maybe.withDefault "/assets/images/default-user.png" review.user.image
            , description = "profile picture"
            }
        ]
    , row []
        [ el [] <| Element.text review.subject.title
        , image []
            { src = Maybe.withDefault "/assets/images/default-subject.png" review.subject.image
            , description = "subject picture"
            }
        ]
    , row []
        [ el [] <| Element.text (String.fromInt review.stars ++ "/5")
        , el [] <| Element.text <|
          Maybe.withDefault "(this review has no text)" review.text        ]
    ]
