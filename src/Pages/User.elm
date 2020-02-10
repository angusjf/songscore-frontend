module Pages.User exposing (..)

import Session
import Styles as S
import Page exposing (Page)
import List exposing (map)
import User exposing (User)
import Review exposing (Review)
import Elements.Review as Review
import Api
import Http
import Route

type alias Model = 
 { reviews : Maybe (List Review)
 , user : Maybe User
 }

type Msg
  = GotReviews (Result Http.Error (List Review))
  | GotUser (Result Http.Error User)

init : Session.Data -> String -> (Model, Session.Data, Cmd Msg)
init session username =
  let
    model =
      { reviews = Nothing
      , user = Nothing
      }
  in
    (model, session, Api.getUser session.userAndToken username GotUser)

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of 
    GotReviews result ->
      case Debug.log "£££££" result of 
        Ok reviews -> ({ model | reviews = Just reviews}, session, Cmd.none)
        Err _ -> (model, session, Cmd.none) -- TODO
    GotUser result ->
      case result of
        Ok user ->
          ( { model | user = Just user}
          , session
          , Api.getUserReviews session.userAndToken user GotReviews
          )
        Err _ -> (model, session, Cmd.none) -- TODO

view : Session.Data -> Model -> Page Msg
view session model =
  { title =
      case model.user of
        Just user -> user.username
        Nothing -> "Loading"
  , body = 
      S.page <|
        [ S.loading model.user <|
            \user -> S.userProfile user.username user.image
        , S.loading model.reviews <|
            \reviews ->
              S.contentList <|
                map
                  (\review -> 
                    Review.view
                      { review = review
                      , session = session
                      , onDelete = Nothing
                      , onLike = Nothing
                      , onDislike = Nothing
                      , onComment = Nothing
                      }
                  )
                  reviews
        ]
  }
