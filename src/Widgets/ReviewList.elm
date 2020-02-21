module Widgets.ReviewList exposing (..)

import Element exposing (Element)
import Styles as S
import Browser
import Review exposing (Review, Comment)
import Session
import Api
import Http
import Time
import Task

type alias Model = 
  { reviews : List (Review, String)
  , currentTime : Maybe Time.Posix
  }

type Msg
  = OnDelete Review
  | OnLike Review
  | OnDislike Review
  | OnCommentSubmit Review String
  | ReviewDeleted (Result Http.Error Review)
  | ReviewLiked (Result Http.Error Review)
  | ReviewDisliked (Result Http.Error Review)
  | CommentSubmitted (Result Http.Error Review)
  | OnReviewCommentChanged Review String
  | GotTime Time.Posix

init : Session.Data -> List Review -> (Model, Session.Data, Cmd Msg)
init session reviews =
  ( { reviews = setReviews reviews, currentTime = Nothing }
  , session
  , Task.perform GotTime Time.now 
  )

view : Session.Data -> Model -> Element Msg
view session model =
  case model.currentTime of
    Just now ->
      S.contentList <|
        List.map (viewReviewAndComment session now) model.reviews
    Nothing ->
      S.contentList [ S.text "loading time..." ]

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case Debug.log "update: " msg of
    OnDelete review ->
      case session.userAndToken of
        Just uAndT ->
          (model, session, Api.deleteReview uAndT review ReviewDeleted)
        Nothing -> 
          (model, session, Cmd.none)
    OnLike review ->
      case session.userAndToken of
        Just uAndT ->
          (model, session, Api.postLike uAndT review ReviewLiked)
        Nothing -> 
          (model, session, Cmd.none)
    OnDislike review ->
      case session.userAndToken of
        Just uAndT ->
          (model, session, Api.postDislike uAndT review ReviewDisliked)
        Nothing -> 
          (model, session, Cmd.none)
    OnCommentSubmit review comment ->
      case session.userAndToken of
        Just uAndT ->
          (model, session, Api.postComment uAndT review comment CommentSubmitted)
        Nothing -> 
          (model, session, Cmd.none)
    ReviewDeleted result ->
      case result of
        Ok review ->
          ( { model | reviews = deleteReview review model.reviews}
          , session
          , Cmd.none
          )
        Err _ -> (model, session, Cmd.none)
    ReviewLiked result ->
      case result of
        Ok review ->
          ( { model | reviews = updateReview review model.reviews }
          , session
          , Cmd.none
          )
        Err _ -> (model, session, Cmd.none)
    ReviewDisliked result ->
      case result of
        Ok review ->
          ( { model | reviews = updateReview review model.reviews }
          , session
          , Cmd.none
          )
        Err _ -> (model, session, Cmd.none)
    CommentSubmitted result ->
      case result of
        Ok review ->
          ( { model | reviews = updateReview review model.reviews }
          , session
          , Cmd.none
          )
        Err _ -> (model, session, Cmd.none)
    OnReviewCommentChanged review newComment ->
      ( { model | reviews = setComment review newComment model.reviews }
      , session
      , Cmd.none
      )
    GotTime posix ->
      ({ model | currentTime = Just posix }, session, Cmd.none)

setComment : Review -> String -> List (Review, String) -> List (Review, String)
setComment review newComment dict =
  setIf (\(r, c) -> r.id == review.id) (review, newComment) dict

deleteReview : Review -> List (Review, String) -> List (Review, String)
deleteReview review dict =
  List.filter (\(r, c) -> r.id /= review.id) dict

updateReview : Review -> List (Review, String) -> List (Review, String)
updateReview review dict =
  setIf (\(r, c) -> r.id == review.id) (review, "") dict

setIf : (a -> Bool) -> a -> List a -> List a
setIf pred elem list =
  case list of
    x::xs ->
      case pred x of
        True -> elem :: setIf pred elem xs
        False -> x :: setIf pred elem xs
    [] -> []

viewReviewAndComment : Session.Data -> Time.Posix -> (Review, String) -> Element Msg
viewReviewAndComment session now (review, newComment) =
  S.viewReview
    (Maybe.map .user session.userAndToken)
    review
    newComment
    now
    (OnDelete review)
    (OnLike review)
    (OnDislike review)
    (OnReviewCommentChanged review)
    (OnCommentSubmit review)

setReviews : List Review -> List (Review, String)
setReviews reviews = List.map (\r -> (r, "")) reviews

addReview : Review -> List (Review, String) -> List (Review, String)
addReview review model = (review, "") :: model
