module Pages.Review exposing (..)

import Session
import Styles as S
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
  { title = Maybe.withDefault "Loading..." <|
        Maybe.map (\x -> x.user.username ++ "'s review") model.review 
  , body = S.loading model.review Review.view
  }
