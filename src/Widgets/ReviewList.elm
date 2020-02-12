module Widgets.ReviewList exposing (..)

import Element exposing (Element)
import Styles as S
import Browser
import Review exposing (Review, Comment)
import Session
import Api
import Http

type alias Model = List (Review, String)

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

init : Session.Data -> List Review -> (Model, Session.Data, Cmd Msg)
init session reviews = (setReviews reviews, session, Cmd.none)

view : Session.Data -> Model -> Element Msg
view session model =
  S.contentList <|
    List.map (viewReviewAndComment session) model

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of
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
          (model, session, Api.postLike uAndT review ReviewDisliked)
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
        Ok review -> (deleteReview review model, session, Cmd.none)
        Err _ -> (model, session, Cmd.none)
    ReviewLiked result ->
      case result of
        Ok review -> (deleteReview review model, session, Cmd.none)
        Err _ -> (model, session, Cmd.none)
    ReviewDisliked result ->
      case result of
        Ok review -> (deleteReview review model, session, Cmd.none)
        Err _ -> (model, session, Cmd.none)
    CommentSubmitted result ->
      case result of
        Ok review -> (deleteReview review model, session, Cmd.none)
        Err _ -> (model, session, Cmd.none)
    OnReviewCommentChanged review newComment ->
      (setComment review newComment model, session, Cmd.none)

setComment : Review -> String -> Model -> Model
setComment review newComment dict =
  setIf (\(r, c) -> r.id == review.id) (review, newComment) dict

deleteReview : Review -> Model -> Model
deleteReview review dict =
  List.filter (\(r, c) -> r.id == review.id) dict

updateReview : Review -> Model -> Model
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

viewReviewAndComment : Session.Data -> (Review, String) -> Element Msg
viewReviewAndComment session (review, newComment) =
  S.viewReview
    (Maybe.map .user session.userAndToken)
    review
    newComment
    (OnDelete review)
    (OnLike review)
    (OnDislike review)
    (OnReviewCommentChanged review)
    (OnCommentSubmit review)

setReviews : List Review -> Model
setReviews reviews = List.map (\r -> (r, "")) reviews

addReview : Review -> Model -> Model
addReview review model = (review, "") :: model
