module Session exposing (..)

import Browser.Navigation as Nav
import User exposing (..)
import Api exposing (UserAndToken)

type alias Data =
  { userAndToken : Maybe UserAndToken
  , key : Nav.Key
  }

fromNavKey : Nav.Key -> Data
fromNavKey key =
    { userAndToken = Nothing
    , key = key
    }
