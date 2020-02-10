module Pages.User exposing (..)

import Session
import Styles as S
import Page exposing (Page)
import List exposing (map)
import User exposing (User)
import Review exposing (Review)
import Api
import Http
import Route

type alias Model = 
 { session : Session.Data
 , reviews : Maybe (List Review)
 , user : Maybe User
 }

type Msg
  = GotReviews (Result Http.Error (List Review))
  | GotUser (Result Http.Error User)

init : Session.Data -> String -> (Model, Cmd Msg)
init session username =
  let
    model =
      { session = session
      , reviews = Nothing
      , user = Nothing
      }
  in
    (model, Api.getUser session.userAndToken username GotUser)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    GotReviews result ->
      case result of 
        Ok reviews -> ({ model | reviews = Just reviews}, Cmd.none)
        Err _ -> (model, Cmd.none)
    GotUser result ->
      case result of
        Ok user ->
          ( { model | user = Just user}
          , Api.getUserReviews model.session.userAndToken user GotReviews
          )
        Err _ -> (model, Cmd.none) -- TODO

view : Model -> Page Msg
view model =
  { title =
      case model.user of
        Just user -> user.username
        Nothing -> "Loading"
  , body = 
      S.page <|
        [ S.loading model.user <|
            \user -> S.userProfile user.username user.image
        , S.loading model.reviews <|
            \reviews -> S.contentList <| map Review.view reviews
        ]
  }
