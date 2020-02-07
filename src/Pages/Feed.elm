module Pages.Feed exposing (..)

import Session
import Element
import Page exposing (Page)

type alias Model = 
 { session : Session.Data
 }

type Msg = None

init : Session.Data -> (Model, Cmd Msg)
init session =
  let
    model =
      { session = session
      }
  in
    (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

view : Model -> Page Msg
view model =
  { title = "Feed"
  , body = Element.text "feed!"
  }
{-
  column []
    [ viewNewReviewForm model.newReviewForm
    , case model.reviews of
        Just reviews ->
          column [] <| map viewReview reviews
        Nothing -> text "loading..."
    ]
    -}
