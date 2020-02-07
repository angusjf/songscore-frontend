module Api exposing (..)

import Http
import Json.Encode as E
import Json.Decode as D
import User exposing (User)

apiRoot : String
apiRoot = "" -- "https://songscore.herokuapp.com"

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

{-

getReviews : String -> Cmd Msg
getReviews token =
  Jwt.Http.get
    token
    { url = apiRoot ++ "/api/reviews"
    , expect = Http.expectJson GotReviews (D.list reviewDecoder)
    }

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
