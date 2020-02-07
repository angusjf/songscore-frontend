module User exposing (..)

import Json.Decode as D
import Json.Encode as E

type alias User =
  { id : Int
  , image : Maybe String
  , username : String
  }

decoder : D.Decoder User
decoder =
  D.map3 User
    (D.field "id" D.int)
    (D.maybe (D.field "image" D.string))
    (D.field "username" D.string)

encode : User -> E.Value
encode user =
  let
    image = case user.image of
      Just img -> E.string img
      Nothing -> E.null
  in
    E.object
      [ ("id", E.int user.id)
      , ("username", E.string user.username)
      , ("image", image)
      ]
