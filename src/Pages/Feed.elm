module Pages.Feed exposing (..)

import Session
import Element as E exposing (Element)
import Page exposing (Page)
import Widgets.NewReviewForm as NRF
import Widgets.ReviewList as RL exposing (ReviewList)
import List exposing (map)
import Review exposing (Review)
import Api
import Http
import Route
import Styles as S
import Time

type alias Model = 
 { nrf : NRF.Form Msg
 , feed : ReviewList
 }

type Msg
  = OnNRFSubmitPressed
  | GotFeed (Result Http.Error (List Review))
  | NRFMsg NRF.Msg
  | GotNewReview (Result Http.Error Review)
  | RLMsg RL.Msg

init : Session.Data -> (Model, Session.Data, Cmd Msg)
init session =
  let
    (rl, rlSession, rlCmd) = RL.init session
    model =
      { nrf = NRF.init NRFMsg OnNRFSubmitPressed
      , feed = rl
      }
  in
    case session.userAndToken of
      Just uAndT ->
        ( model
        , session
        , Cmd.batch
            [ Api.getFeed uAndT GotFeed
            , Cmd.map RLMsg rlCmd
            ]
        )
      Nothing ->
        ( model
        , session
        , Route.goTo session.key Route.Login
        )

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of 
    OnNRFSubmitPressed ->
      case session.userAndToken of
        Just uAndT -> 
          case NRF.convertToReview session.currentTime model.nrf uAndT.user of
            Just newReview ->
              (model, session, Api.postReview uAndT newReview GotNewReview)
            Nothing -> 
              (model, session, Cmd.none)
        Nothing -> (model, session, Cmd.none)
    GotNewReview result -> 
      case result of
        Ok review ->
          let
            feed = model.feed
            newFeed = RL.add review model.feed
          in
            ( { model
                | feed = newFeed
                , nrf = NRF.init NRFMsg OnNRFSubmitPressed
              }
            , session
            , Cmd.none
            )
        Err _ ->
          (model, { session | userAndToken = Nothing }, Cmd.none)
    GotFeed result ->
      case result of 
        Ok reviews ->
          ( { model
              | feed = RL.fromList reviews
            }
          , session
          , Cmd.none
          )
        Err _ ->
          (model, { session | userAndToken = Nothing }, Cmd.none)
    NRFMsg nrfMsg ->
      let 
        (nrfModel, cmd) = NRF.update nrfMsg model.nrf
      in
        ({ model | nrf = nrfModel }, session, cmd)
    RLMsg rlMsg ->
      let 
        (rlModel, newSession, rlCmd) = RL.update rlMsg model.feed session
      in
        ( { model | feed = rlModel }
        , newSession
        , Cmd.map RLMsg rlCmd
        )

view : Session.Data -> Model -> Page Msg
view session model =
  { title = "Feed"
  , body = 
      E.column [S.spacingMedium]
        [ NRF.view model.nrf
        , E.map RLMsg <| RL.view session model.feed
        ]
  }
