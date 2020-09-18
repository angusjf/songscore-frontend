module Widgets.ReviewList exposing ( ReviewList, init, view, update
                                   , Msg, fromList, add
                                   )

import Element exposing (Element)
import Styles as S
import Browser
import Review exposing (Review, Comment)
import Session
import Api
import Http
import Time
import Task

type alias ReviewList = Maybe (List (Review, String))

type Msg
  = OnDelete Review
  | OnLike Review
  | OnDislike Review
  | OnCommentSubmit Review String
  | ReviewDeleted (Result Http.Error Review)
  | ReviewUpdated (Result Http.Error Review)
  | OnReviewCommentChanged Review String

add : Review -> ReviewList -> ReviewList
add review rl =
  case rl of
    Just reviews ->
      Just <| (review, "") :: reviews
    Nothing ->
      fromList [review]

init : Session.Data -> (ReviewList, Session.Data, Cmd Msg)
init session =
  ( Nothing
  , session
  , Cmd.none
  )

view : Session.Data -> ReviewList -> Element Msg
view session model =
  case model of
    Just reviews ->
      S.contentList <|
        List.map (viewReviewAndComment session) reviews
    Nothing ->
      S.text "loading..."

update : Msg -> ReviewList -> Session.Data -> (ReviewList, Session.Data, Cmd Msg)
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
          (model, session, Api.postLike uAndT review ReviewUpdated)
        Nothing -> 
          (model, session, Cmd.none)
    OnDislike review ->
      case session.userAndToken of
        Just uAndT ->
          (model, session, Api.postDislike uAndT review ReviewUpdated)
        Nothing -> 
          (model, session, Cmd.none)
    OnCommentSubmit review comment ->
      case session.userAndToken of
        Just uAndT ->
          (model, session, Api.postComment uAndT review comment ReviewUpdated)
        Nothing -> 
          (model, session, Cmd.none)
    ReviewDeleted result ->
      case result of
        Ok review ->
          ( model |> Maybe.map (deleteReview review)
          , session
          , Cmd.none
          )
        Err e -> Debug.todo <| Debug.toString e
    ReviewUpdated result ->
      case result of
        Ok review ->
          ( model |> Maybe.map (updateReview review)
          , session
          , Cmd.none
          )
        Err e -> Debug.todo <| Debug.toString e
    OnReviewCommentChanged review newComment ->
      ( model |> Maybe.map (setComment review newComment)
      , session
      , Cmd.none
      )

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

viewReviewAndComment : Session.Data -> (Review, String) -> Element Msg
viewReviewAndComment session (review, newComment) =
  S.viewReview
    (Maybe.map .user session.userAndToken)
    review
    newComment
    session.currentTime
    (OnDelete review)
    (OnLike review)
    (OnDislike review)
    (OnReviewCommentChanged review)
    (OnCommentSubmit review)

fromList : List Review -> ReviewList
fromList reviews = Just <| List.map (\r -> (r, "")) reviews

addReview : Review -> List (Review, String) -> List (Review, String)
addReview review model = (review, "") :: model
