port module Session exposing (..)

import Browser.Navigation as Nav
import User exposing (..)
import Api exposing (UserAndToken)
import Json.Decode as D

type alias Data =
  { userAndToken : Maybe UserAndToken
  , key : Nav.Key
  }

create : Maybe UserAndToken -> Nav.Key -> Data
create uAndT key =
    { userAndToken = uAndT
    , key = key
    }

port store : Maybe UserAndToken -> Cmd msg
