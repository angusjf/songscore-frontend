port module Session exposing (..)

import Browser.Navigation as Nav
import User exposing (..)
import Api exposing (UserAndToken)
import Json.Decode as D
import Time

type alias Data =
  { userAndToken : Maybe UserAndToken
  , key : Nav.Key
  , currentTime : Time.Posix
  , unreadNotifications : Maybe Bool
  }

store : Data -> Cmd msg
store data = storePort (data.userAndToken, Time.posixToMillis data.currentTime)

port storePort : (Maybe UserAndToken, Int) -> Cmd msg
