module Pages.Feed exposing (..)

import Session
import Element exposing (column, text)
import Page exposing (Page)
import Widgets.NewReviewForm as NRF
import List exposing (map)
import Review exposing (Review)
import Api
import Http
import Route
import MusicDatabase as MDB
import Styles as S

type alias Model = 
 { session : Session.Data
 , nrf : NRF.Form Msg
 , reviews : Maybe (List Review)
 }

type Msg
  = OnNRFSubmitPressed
  | GotFeed (Result Http.Error (List Review))
  | NRFMsg NRF.Msg
  | GotNewReview (Result Http.Error Review)

init : Session.Data -> (Model, Cmd Msg)
init session =
  let
    model =
      { session = session
      , nrf = NRF.init NRFMsg OnNRFSubmitPressed
      , reviews = Nothing
      }
  in
    case model.session.userAndToken of
      Just uAndT -> (model, Api.getFeed uAndT GotFeed)
      Nothing -> (model, Route.goTo model.session.key Route.Login)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    OnNRFSubmitPressed ->
      case model.session.userAndToken of
        Just uAndT -> 
          case Debug.log "?" <| NRF.convertToReview model.nrf uAndT.user of
            Just newReview -> (model, Api.postReview uAndT newReview GotNewReview)
            Nothing -> (model, Cmd.none)
        Nothing -> (model, Cmd.none)
    GotNewReview result -> 
      case Debug.log "!" result of
        Ok review ->
          let
            newReviews = 
              Just <|
                case model.reviews of
                  Just reviews -> review :: reviews
                  Nothing -> [ review ]
          in
            ( { model
                | reviews = newReviews
                , nrf = NRF.init NRFMsg OnNRFSubmitPressed
              }
            , Cmd.none
            )
        Err _ -> (model, Cmd.none)
    GotFeed result ->
      case result of 
        Ok reviews -> ({ model | reviews = Just reviews}, Cmd.none)
        Err _ -> (model, Cmd.none)
    NRFMsg nrfMsg ->
      let 
        (nrfModel, cmd) = NRF.update model.nrf nrfMsg
      in
        ({ model | nrf = nrfModel }, cmd)

view : Model -> Page Msg
view model =
  { title = "Feed"
  , body = 
      column [S.spacingMedium]
        [ NRF.view model.nrf
        , case model.reviews of
            Just reviews ->
              column [S.spacingMedium] <| map Review.view reviews
            Nothing -> text "loading..."
        ]
  }
