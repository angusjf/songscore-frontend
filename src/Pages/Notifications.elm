module Pages.Notifications exposing (..)

import Element as E exposing (Element)
import Page exposing (Page)
import Browser
import Api
import Http
import User
import Session
import Route
import Styles as S
import Notification exposing (Notification)

type alias Model =
  { notifications : Maybe (List Notification)
  }

type Msg
  = GotNotifications (Result Http.Error (List Notification))
  | GotNewNotificationsCheck (Result Http.Error (List Notification))

init : Session.Data -> (Model, Session.Data, Cmd Msg)
init session =
  let
    model =
      { notifications = Nothing
      }
    cmds =
      case session.userAndToken of
        Just uAndT ->
          [ Api.getNotifications uAndT GotNotifications
          , Api.postNotifications uAndT GotNewNotificationsCheck
          ]
        Nothing ->
          []
  in
    (model, session, Cmd.batch cmds)

view : Session.Data -> Model -> Page Msg
view session model = 
  { title = "Notifications"
  , body =
      case session.userAndToken of
        Nothing ->
          E.text "sign in!"
        Just uAndT ->
          E.column [ S.spacingMedium ] <|
            case model.notifications of
              Just [] ->
                [ S.text "no notifications yet!"
                ]
              Just notifs ->
                List.map (viewNotification uAndT.user.username) notifs
              Nothing ->
                [ S.text "loading..."
                ]
  }

viewNotification : String -> Notification -> Element Msg
viewNotification username notif =
  E.link
    [ S.spacingMedium ]
    { label = E.text notif.text
    , url = "/users/" ++ username ++ "/reviews/" ++ (String.fromInt notif.reviewId)
    }

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of
    GotNotifications result ->
      case result of
        Ok notifs ->
          ( { model | notifications = Just notifs }, session, Cmd.none)
        Err e ->
          Debug.todo <| Debug.toString e
    GotNewNotificationsCheck result ->
      case result of
        Ok new ->
          ( model
          , { session | unreadNotifications = Just <| not <| List.isEmpty new }
          , Cmd.none
          )
        Err e ->
          Debug.todo <| Debug.toString e
