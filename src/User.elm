module User exposing (..)

import Json.Decode as D
import Json.Encode as E

type alias User =
  { id : Maybe Int
  , image : Maybe String
  , username : String
  }

decoder : D.Decoder User
decoder =
  D.map3 User
    (D.field "id" (D.maybe D.int))
    (D.field "image" (D.maybe D.string))
    (D.field "username" D.string)

encode : User -> E.Value
encode user =
  let
    id = case user.id of
      Just i -> E.int i
      Nothing -> E.null
    image = case user.image of
      Just img -> E.string img
      Nothing -> E.null
  in
    E.object
      [ ("id", id)
      , ("username", E.string user.username)
      , ("image", image)
      ]
