module Review exposing (..)

import Json.Decode as D
import Json.Encode as E
import Subject exposing (Subject)
import User exposing (User)

type alias Review =
  { id : Maybe Int
  , text : Maybe String
  , stars : Int
  , user : User
  , subject : Subject
  , comments : List Comment
  , likes : List User
  , dislikes : List User
  }

type alias Comment =
  { id : Maybe Int
  , user : User
  , text : String
  }

decoder : D.Decoder Review
decoder =
  D.map8 Review
    (D.maybe (D.field "id" D.int))
    (D.maybe (D.field "text" D.string))
    (D.field "stars" D.int)
    (D.field "user" User.decoder)
    (D.field "subject" Subject.decoder)
    (D.field "comments" (D.list decodeComment))
    (D.field "likes" (D.list User.decoder))
    (D.field "dislikes" (D.list User.decoder))

decodeComment : D.Decoder Comment
decodeComment =
  D.map3 Comment
    (D.maybe (D.field "id" D.int))
    (D.field "user" User.decoder)
    (D.field "text" D.string)

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
      , ("likes", (E.list User.encode review.likes))
      , ("dislikes", (E.list User.encode review.dislikes))
      , ("comment", (E.list encodeComment review.comments))
      ]

encodeComment : Comment -> E.Value
encodeComment comment =
  let
    id =
      case comment.id of
        Just x -> E.int x
        Nothing -> E.null
  in
    E.object
      [ ("id", id)
      , ("text", E.string comment.text)
      , ("user", User.encode comment.user)
      ]
