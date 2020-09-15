module Notification exposing (..)

import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline as P
import Iso8601
import Time

type alias Notification =
  { id : Int
  , reviewId : Int
  , text : String
  , insertedAt : Time.Posix
  }

decoder : D.Decoder Notification
decoder =
  D.succeed Notification
    |> P.required "id" D.int
    |> P.required "review_id" D.int
    |> P.required "text" D.string
    |> P.required "inserted_at" Iso8601.decoder
