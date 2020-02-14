module Api exposing (..)

import Http
import Json.Encode as E
import Json.Decode as D
import User exposing (User)
import Review exposing (Review)
import Jwt.Http
import Url.Builder as Builder

apiRoot : String
apiRoot = ""
-------- "https://songscore.herokuapp.com"
-------- "http://localhost:8081"
-------- ""

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

maybeTokenGet : Maybe UserAndToken -> { expect : Http.Expect msg, url : String } -> Cmd msg
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

postLike : UserAndToken -> Review -> (Result Http.Error Review -> msg) -> Cmd msg
postLike uAndT review msg =
  let
    id = String.fromInt <| Maybe.withDefault (-1) review.id 
  in
    Jwt.Http.post
      uAndT.token
      { url = apiRoot ++ "/api/reviews/" ++ id ++ "/like"
      , body = Http.emptyBody
      , expect = Http.expectJson msg Review.decoder
      } 

postDislike : UserAndToken -> Review -> (Result Http.Error Review -> msg) -> Cmd msg
postDislike uAndT review msg =
  let
    id = String.fromInt <| Maybe.withDefault (-1) review.id 
  in
    Jwt.Http.post
      uAndT.token
      { url = apiRoot ++ "/api/reviews/" ++ id ++ "/dislike"
      , body = Http.emptyBody
      , expect = Http.expectJson msg Review.decoder
      } 

postComment : UserAndToken -> Review -> String -> (Result Http.Error Review -> msg) -> Cmd msg
postComment uAndT review comment msg =
  let
    body =
      Review.encodeComment
        { id = Nothing, text = comment, user = uAndT.user }
    id = String.fromInt <| Maybe.withDefault (-1) review.id 
  in
    Jwt.Http.post
      uAndT.token
      { url = apiRoot ++ "/api/reviews/" ++ id ++ "/comments"
      , body = Http.jsonBody body
      , expect = Http.expectJson msg Review.decoder
      } 

deleteReview : UserAndToken -> Review -> (Result Http.Error Review -> msg) -> Cmd msg
deleteReview uAndT review msg =
  let
    id = Maybe.withDefault "NULL" <| Maybe.map String.fromInt review.id
  in
    Jwt.Http.delete
      uAndT.token
      { url = apiRoot ++ "/api/reviews/" ++ id
      , expect = Http.expectJson msg Review.decoder
      } 

getUsernameAvailability : String -> (Result Http.Error Bool -> msg) -> Cmd msg
getUsernameAvailability username msg =
  Http.get
    { url = apiRoot ++ "/api/users/" ++ username ++ "/available"
    , expect = Http.expectJson msg D.bool
    }

type alias SubjectResult =
  { name : String
  , artist : String
  , imageUrl : String
  , kind : String
  , spotifyId : String
  }

searchSubjects : String -> Int
      -> (Result Http.Error (List SubjectResult) -> msg) -> Cmd msg
searchSubjects query limit msg =
  let
    url =
      Builder.crossOrigin
        apiRoot
        [ "subjects", "search" ]
        [ Builder.string "q" query
        , Builder.int "limit" limit
        ]
  in 
    Http.get
      { url = url
      , expect = Http.expectJson msg subjectResultsDecoder
      }

subjectResultsDecoder : D.Decoder (List SubjectResult)
subjectResultsDecoder = 
  D.at [ "tracks", "items"] <|
    D.list <|
      D.map5 SubjectResult
       (D.field "name" D.string)
       (D.field "artists" <| D.index 0 <| D.field "name" D.string)
       (D.at ["album", "images"] <| D.index 0 <| D.field "url" D.string)
       (D.field "type" D.string)
       (D.field "id" D.string)
