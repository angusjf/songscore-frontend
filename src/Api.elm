module Api exposing (..)

import Http
import Json.Encode as E
import Json.Decode as D
import User exposing (User)
import Review exposing (Review)
import Jwt.Http

apiRoot : String
apiRoot =
  case 1 of
    0 -> "https://songscore.herokuapp.com"
    _ -> "http://localhost:8081"

type alias Credentials =
  { username : String
  , password : String 
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

postUser : Credentials -> (Result Http.Error UserAndToken -> msg) -> Cmd msg
postUser creds msg =
  let
    body =
      Http.jsonBody <|
      E.object <|
        [ ("username", E.string creds.username)
        , ("password", E.string creds.password)
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
    { url = apiRoot ++ "/api/feed/" ++ (String.fromInt userAndToken.user.id)
    , expect = Http.expectJson msg (D.list Review.decoder)
    }

{-


postReview : String -> Review -> Cmd Msg
postReview token review =
  Jwt.Http.post
    token
    { url = apiRoot ++ "/api/reviews"
    , body = Http.jsonBody (encodeReview review)
    , expect = Http.expectJson GotNewReview reviewDecoder
    }

postLogin : String -> String -> Cmd Msg
postLogin username password =
  let
    body =
      Http.jsonBody <|
      E.object <|
        [ ("username", E.string username)
        , ("password", E.string password)
        ]
  in
    Http.post
      { url = apiRoot ++ "/api/auth"
      , body = body
      , expect = Http.expectString GotToken
      }

postSignUp : String -> String -> Cmd Msg
postSignUp username password =
  let
    body =
      Http.jsonBody <|
      E.object <|
        [ ("username", E.string username)
        , ("password", E.string password)
        ]
  in
    Http.post
      { url = apiRoot ++ "/api/users"
      , body = body
      , expect = Http.expectJson SignedUp userDecoder
      }

getMe : String -> Cmd Msg
getMe token =
  Jwt.Http.get
    token
    { url = apiRoot ++ "/api/me"
    , expect = Http.expectJson GotMe userDecoder
    }

emptyNewReviewForm : NewReviewForm Msg
emptyNewReviewForm =
  { text = Nothing
  , stars = Nothing
  , subject = Nothing
  , subjectQuery = Nothing
  , onPress = NewReviewPostClicked
  , onChange = NewReviewBoxTextChanged
  , starsRadioChanged = NewReviewFormStarsRadioChanged
  , onSubjectQueryChanged = NewReviewFormSubjectQueryChanged
  }
-}
