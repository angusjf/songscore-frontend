module Api exposing (..)

import Http
import Json.Encode as E
import Json.Decode as D
import User exposing (User)
import Review exposing (Review)
import Jwt.Http

apiRoot : String
apiRoot =
  case 2 of
    0 -> "https://songscore.herokuapp.com"
    1 -> "http://localhost:8081"
    _ -> ""

type alias Credentials =
  { username : String
  , password : String 
  }

type alias NewUser =
  { username : String
  , password : String 
  , image : String
  }

type alias UserAndToken =
  { user : User
  , token : String
  }

userAndTokenDecoder : D.Decoder UserAndToken
userAndTokenDecoder =
  D.map2 UserAndToken
    (D.field "user" User.decoder)
    (D.field "token" D.string)

postUser : NewUser -> (Result Http.Error UserAndToken -> msg) -> Cmd msg
postUser newUser msg =
  let
    body =
      Http.jsonBody <|
      E.object <|
        [ ("username", E.string newUser.username)
        , ("password", E.string newUser.password)
        , ("image", E.string newUser.image)
        ]
  in
    Http.post
      { url = apiRoot ++ "/api/users"
      , body = body
      , expect = Http.expectJson msg userAndTokenDecoder
      }

postLogin : Credentials -> (Result Http.Error UserAndToken -> msg) -> Cmd msg
postLogin creds msg =
  let
    body =
      Http.jsonBody <|
      E.object <|
        [ ("username", E.string creds.username)
        , ("password", E.string creds.password)
        ]
  in
    Http.post
      { url = apiRoot ++ "/api/auth"
      , body = body
      , expect = Http.expectJson msg userAndTokenDecoder
      }

getFeed : UserAndToken -> (Result Http.Error (List Review) -> msg) -> Cmd msg
getFeed userAndToken msg =
  Jwt.Http.get
    userAndToken.token
    { url = apiRoot ++ "/api/feeds/" ++ userAndToken.user.username
    , expect = Http.expectJson msg (D.list Review.decoder)
    }

postReview : UserAndToken -> Review -> (Result Http.Error Review -> msg) -> Cmd msg
postReview userAndToken review msg =
  Jwt.Http.post
    userAndToken.token
    { url = apiRoot ++ "/api/reviews"
    , body = Http.jsonBody (Review.encode review)
    , expect = Http.expectJson msg Review.decoder
    }

maybeTokenGet : Maybe UserAndToken ->  { expect : Http.Expect msg, url : String } -> Cmd msg
maybeTokenGet userAndToken =
  case userAndToken of 
    Just uAndT -> Jwt.Http.get uAndT.token
    Nothing -> Http.get

getUserReviews : Maybe UserAndToken -> User -> (Result Http.Error (List Review) -> msg) -> Cmd msg
getUserReviews userAndToken user msg =
  maybeTokenGet userAndToken
    { url = apiRoot ++ "/api/users/" ++ user.username ++ "/reviews"
    , expect = Http.expectJson msg (D.list Review.decoder)
    }

getReview : Maybe UserAndToken -> Int -> (Result Http.Error Review -> msg) -> Cmd msg
getReview userAndToken id msg =
  maybeTokenGet userAndToken
    { url = apiRoot ++ "/api/reviews/" ++ (String.fromInt id)
    , expect = Http.expectJson msg Review.decoder
    }

getUser : Maybe UserAndToken -> String -> (Result Http.Error User -> msg) -> Cmd msg
getUser userAndToken username msg =
  maybeTokenGet userAndToken
    { url = apiRoot ++ "/api/users/" ++ username
    , expect = Http.expectJson msg User.decoder
    }
