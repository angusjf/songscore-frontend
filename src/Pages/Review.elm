module Pages.Review exposing (..)

import Session
import Styles as S
import Page exposing (Page)
import Review exposing (Review)
import Api
import Http
import Route

type alias Model = 
 { review : Maybe Review
 }

type Msg
  = GotReview (Result Http.Error Review)

init : Session.Data -> String -> Int -> (Model, Session.Data, Cmd Msg)
init session username id =
  let
    model =
      { review = Nothing
      }
  in
    (model, session, Api.getReview session.userAndToken id GotReview)

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of 
    GotReview result ->
      case result of 
        Ok review -> ({ model | review = Just review}, session, Cmd.none)
        Err _ -> (model, session, Cmd.none)

view : Session.Data -> Model -> Page Msg
view session model =
  { title =
      Maybe.withDefault "Loading..." <|
        Maybe.map (\x -> x.user.username ++ "'s review") model.review 
  , body = S.text "NOT DONE!"
  }
