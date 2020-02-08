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
  | OnStarsChanged Int
  | OnSubjectQueryChanged String
  | GotFeed (Result Http.Error (List Review))
  | GotNewReview (Result Http.Error Review)

init : Session.Data -> (Model, Cmd Msg)
init session =
  let
    model =
      { session = session
      , newReviewForm = clearReviewForm
      , reviews = Nothing
      }
  in
    case model.session.userAndToken of
      Just uAndT -> (model, Api.getFeed uAndT GotFeed)
      Nothing -> (model, Route.goTo model.session.key Route.Login)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    OnSubjectChanged new ->
      let
        newForm = NewReviewForm.setText model.newReviewForm new
      in
        ({ model | newReviewForm = newForm }, Cmd.none)
    OnStarsChanged n ->
      let
        newForm = NewReviewForm.setStars model.newReviewForm n
      in
        ({ model | newReviewForm = newForm }, Cmd.none)
    OnSubjectQueryChanged new ->
      let
        newForm = NewReviewForm.setSubjectQuery model.newReviewForm new
      in
        ({ model | newReviewForm = newForm }, Cmd.none)
    OnSubmitPressed ->
      case model.session.userAndToken of
        Just uAndT -> 
          case NewReviewForm.convertToNewReview model.newReviewForm uAndT.user of
            Just newReview -> (model, Api.postReview uAndT newReview GotNewReview)
            Nothing -> (model, Cmd.none)
        Nothing -> (model, Cmd.none)
    GotNewReview result -> 
      case result of
        Ok review ->
          let
            newReviews = 
              Just <|
                case model.reviews of
                  Just reviews -> review :: reviews
                  Nothing -> [ review ]
          in
            ( { model | reviews = newReviews, newReviewForm = clearReviewForm }
            , Cmd.none
            )
        Err _ -> (model, Cmd.none)
    GotFeed result ->
      case (Debug.log ">>>>>>" result) of 
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

clearReviewForm : NewReviewForm Msg
clearReviewForm = 
  { text = Nothing
  , stars = Nothing
  , subject = Nothing
  , subjectQuery = Nothing
  , onPress = OnSubmitPressed
  , onChange = OnSubjectChanged
  , onStarsChanged = OnStarsChanged
  , onSubjectQueryChanged = OnSubjectQueryChanged
  }
