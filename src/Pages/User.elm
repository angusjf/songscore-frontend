module Pages.User exposing (..)

import Session
import Styles as S
import Page exposing (Page)
import List exposing (map)
import User exposing (User)
import Review exposing (Review)
import Element
import Widgets.ReviewList as ReviewList
import Api
import Http
import Route

type alias Model = 
 { user : Maybe User
 , reviewListModel : ReviewList.Model
 }

type Msg
  = GotReviews (Result Http.Error (List Review))
  | GotUser (Result Http.Error User)
  | ReviewListMsg ReviewList.Msg

init : Session.Data -> String -> (Model, Session.Data, Cmd Msg)
init session username =
  let
    (reviewListModel, _, _) = ReviewList.init session []
    model =
      { user = Nothing
      , reviewListModel = reviewListModel
      }
  in
    (model, session, Api.getUser session.userAndToken username GotUser)

stepReviewList : Model ->
                 (ReviewList.Model, Session.Data, Cmd ReviewList.Msg) ->
                 (Model, Session.Data, Cmd Msg)
stepReviewList model (reviewListModel, session, msg) =
  ( { model | reviewListModel = reviewListModel }
  , session, Cmd.map ReviewListMsg msg
  )

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of 
    GotReviews result ->
      case result of 
        Ok reviews ->
          ( { model
              | reviewListModel = ReviewList.setReviews reviews
            }
          , session
          , Cmd.none
          )
        Err _ ->
          (model, session, Cmd.none) -- TODO
    GotUser result ->
      case result of
        Ok user ->
          ( { model | user = Just user}
          , session
          , Api.getUserReviews session.userAndToken user GotReviews
          )
        Err _ -> (model, session, Cmd.none) -- TODO
    ReviewListMsg rlMsg ->
      stepReviewList model <|
        ReviewList.update rlMsg model.reviewListModel session

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
        , Element.map ReviewListMsg <|
            ReviewList.view session model.reviewListModel
        ]
  }
