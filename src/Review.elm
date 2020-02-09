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
  E.row []
    [ E.link []
        { url = "/users/" ++ review.user.username
        , label =
            E.column []
            [ E.image S.mediumSquare
                { src = Maybe.withDefault "/assets/images/default-user.png" review.user.image
                , description = "profile picture"
                }
            , E.text ("@" ++ review.user.username)
            ]
        }
    , E.column []
        [ E.image S.mediumSquare
            { src = Maybe.withDefault "/assets/images/default-subject.png" review.subject.image
            , description = "subject picture"
            }
        , E.el [] <| E.text review.subject.title
        ]
    , E.column []
        [ E.el [] <| viewNStars review.stars
        , E.el [] <| E.text <|
             Maybe.withDefault "(this review has no text)" review.text
        ]
    ]
 
viewNStars : Int -> Element msg
viewNStars n =
  E.row [] <|
    (List.repeat n redStar) ++ (List.repeat (5 - n) greyStar)

redStar : Element msg
redStar =
  E.image []
    { src = "/assets/images/red-star.png"
    , description = "red star"
    }

greyStar : Element msg
greyStar =
  E.image []
    { src = "/assets/images/grey-star.png"
    , description = "grey star"
    }
