module Pages.Review exposing (..)

import Session
import Styles as S
import Page exposing (Page)
import Review exposing (Review)
import Element
import Api
import Http
import Route
import Widgets.ReviewList as ReviewList exposing (ReviewList)

type alias Model = 
 { reviewListModel : ReviewList
 , review : Maybe Review
 }

type Msg
  = GotReview (Result Http.Error Review)
  | ReviewListMsg ReviewList.Msg

init : Session.Data -> String -> Int -> (Model, Session.Data, Cmd Msg)
init session username id =
  let
    (reviewListModel, rlSession, rlCmd) = ReviewList.init session
    model =
      { reviewListModel = reviewListModel
      , review = Nothing
      }
  in
    ( model
    , rlSession
    , Cmd.batch
        [ Api.getReview session.userAndToken id GotReview
        , Cmd.map ReviewListMsg rlCmd
        ]
    )

stepReviewList : Model ->
                 (ReviewList, Session.Data, Cmd ReviewList.Msg) ->
                 (Model, Session.Data, Cmd Msg)
stepReviewList model (reviewListModel, session, msg) =
  ( { model | reviewListModel = reviewListModel }
  , session
  , Cmd.map ReviewListMsg msg
  )

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of 
    GotReview result ->
      case result of 
        Ok review ->
          ({ model
             | reviewListModel = ReviewList.fromList [review]
             , review = Just review
           }
          , session
          , Cmd.none
          )
        Err e ->
          Debug.todo <| Debug.toString e
    ReviewListMsg rlMsg ->
      stepReviewList model <|
        ReviewList.update rlMsg model.reviewListModel session

view : Session.Data -> Model -> Page Msg
view session model =
  { title =
      Maybe.withDefault "Loading..." <|
        Maybe.map (\x -> x.user.username ++ "'s review") model.review 
  , body =
      Element.map ReviewListMsg <|
        ReviewList.view session model.reviewListModel
  }
