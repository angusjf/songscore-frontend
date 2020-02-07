module Pages.Feed exposing (..)

import Session
import Element exposing (column, text)
import Page exposing (Page)
import Widgets.NewReviewForm as NewReviewForm exposing (NewReviewForm)
import List exposing (map)
import Review exposing (Review)
import Api
import Http
import Route

type alias Model = 
 { session : Session.Data
 , newReviewForm : NewReviewForm Msg
 , reviews : Maybe (List Review)
 }

type Msg
  = OnSubmitPressed
  | OnSubjectChanged String
  | OnStarsRadioChanged Int
  | OnSubjectQueryChanged String
  | GotFeed (Result Http.Error (List Review))

init : Session.Data -> (Model, Cmd Msg)
init session =
  let
    model =
      { session = session
      , newReviewForm = 
          { text = Nothing
          , stars = Nothing
          , subject = Nothing
          , subjectQuery = Nothing
          , onPress = OnSubmitPressed
          , onChange = OnSubjectChanged
          , onStarsRadioChanged = OnStarsRadioChanged
          , onSubjectQueryChanged = OnSubjectQueryChanged
          }
      , reviews = Nothing
      }
  in
    case model.session.userAndToken of
      Just uAndT -> (model, Api.getFeed uAndT GotFeed)
      Nothing -> (model, Route.goTo model.session.key Route.Login)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    OnSubmitPressed -> (model, Cmd.none)
    OnSubjectChanged new ->
      let
        newForm = NewReviewForm.setText model.newReviewForm new
      in
        ({ model | newReviewForm = newForm }, Cmd.none)
    OnStarsRadioChanged n ->
      let
        newForm = NewReviewForm.setStars model.newReviewForm n
      in
        ({ model | newReviewForm = newForm }, Cmd.none)
    OnSubjectQueryChanged new ->
      let
        newForm = NewReviewForm.setSubjectQuery model.newReviewForm new
      in
        ({ model | newReviewForm = newForm }, Cmd.none)
    GotFeed result ->
      case result of 
        Ok reviews -> ({ model | reviews = Just reviews}, Cmd.none)
        Err _ -> (model, Cmd.none)

view : Model -> Page Msg
view model =
  { title = "Feed"
  , body = 
      column []
        [ NewReviewForm.view model.newReviewForm
        , case model.reviews of
            Just reviews ->
              column [] <| map Review.view reviews
            Nothing -> text "loading..."
        ]
  }
