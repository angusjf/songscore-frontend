module Pages.Feed exposing (..)

import Session
import Element exposing (column, text)
import Elements.Review as Review
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
 { nrf : NRF.Form Msg
 , reviews : Maybe (List Review)
 }

type Msg
  = OnNRFSubmitPressed
  | GotFeed (Result Http.Error (List Review))
  | NRFMsg NRF.Msg
  | GotNewReview (Result Http.Error Review)

init : Session.Data -> (Model, Session.Data, Cmd Msg)
init session =
  let
    model =
      { nrf = NRF.init NRFMsg OnNRFSubmitPressed
      , reviews = Nothing
      }
  in
    case session.userAndToken of
      Just uAndT -> (model, session, Api.getFeed uAndT GotFeed)
      Nothing -> (model, session, Route.goTo session.key Route.Login)

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of 
    OnNRFSubmitPressed ->
      case session.userAndToken of
        Just uAndT -> 
          case Debug.log "?" <| NRF.convertToReview model.nrf uAndT.user of
            Just newReview ->
              (model, session, Api.postReview uAndT newReview GotNewReview)
            Nothing -> 
              (model, session, Cmd.none)
        Nothing -> (model, session, Cmd.none)
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
            , session
            , Cmd.none
            )
        Err _ -> (model, session, Cmd.none)
    GotFeed result ->
      case result of 
        Ok reviews -> ({ model | reviews = Just reviews }, session, Cmd.none)
        Err _ -> (model, session, Cmd.none)
    NRFMsg nrfMsg ->
      let 
        (nrfModel, cmd) = NRF.update nrfMsg model.nrf
      in
        ({ model | nrf = nrfModel }, session, cmd)

view : Session.Data -> Model -> Page Msg
view session model =
  { title = "Feed"
  , body = 
      column [S.spacingMedium]
        [ NRF.view model.nrf
        , S.loading model.reviews <|
            column [S.spacingMedium] <<
              map
                (\review -> 
                  Review.view
                    { review = review
                    , session = session
                    , onDelete = Nothing
                    , onLike = Nothing
                    , onDislike = Nothing
                    , onComment = Nothing
                    }
                )
        ]
  }
