module Review exposing (..)

import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline as P
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
  , createdAt : Maybe Int
  }

type alias Comment =
  { id : Maybe Int
  , user : User
  , text : String
  }

decoder : D.Decoder Review
decoder =
  D.succeed Review
    |> P.required "id" (D.nullable D.int)
    |> P.optional "text" (D.maybe D.string) Nothing
    |> P.required "stars" D.int
    |> P.required "user" User.decoder
    |> P.required "subject" Subject.decoder
    |> P.required "comments" (D.list decodeComment)
    |> P.required "likes" (D.list User.decoder)
    |> P.required "dislikes" (D.list User.decoder)
    |> P.required "createdAt" (D.nullable D.int)

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
    createdAt =
      case review.createdAt of
        Just c -> E.int c
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
      , ("createdAt", createdAt)
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
