module Pages.Review exposing (..)

import Session
import Element exposing (column, text)
import Page exposing (Page)
import Review exposing (Review)
import Api
import Http
import Route

type alias Model = 
 { session : Session.Data
 , review : Maybe Review
 }

type Msg
  = GotReview (Result Http.Error Review)

init : Session.Data -> String -> Int -> (Model, Cmd Msg)
init session username id =
  let
    model =
      { session = session
      , review = Nothing
      }
  in
    (model, Api.getReview session.userAndToken id GotReview)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    GotReview result ->
      case result of 
        Ok review -> ({ model | review = Just review}, Cmd.none)
        Err _ -> (model, Cmd.none)

view : Model -> Page Msg
view model =
  { title =
      case model.review of
        Just review -> review.user.username ++ "'s review"
        Nothing -> "Loading"
  , body =
      case model.review of
        Just review -> Review.view review
        Nothing -> Element.text "Loading..."
  }
