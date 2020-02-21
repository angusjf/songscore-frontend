module Pages.Settings exposing (..)

import Element as E
import Page exposing (Page)
import Element.Input as Input
import Browser
import Api
import Http
import User
import Session
import Route
import Styles as S
import User exposing (User)

type alias Model =
  { usernameAvailable : Maybe Bool
  , newUsername : String
  , feedback : String
  }

type Msg
  = UsernameChanged String
  | SaveClicked
  | GotChangedUser (Result Http.Error Api.UserAndToken)
  | GotUsernameAvailable (Result Http.Error Bool)

init : Session.Data -> (Model, Session.Data, Cmd Msg)
init session =
  let
    model =
      { usernameAvailable = Nothing
      , newUsername =
          Maybe.withDefault "not logged in :(" <|
            Maybe.map (\x -> x.user.username) session.userAndToken
      , feedback = ""
      }
  in
    (model, session, Cmd.none)

view : Session.Data -> Model -> Page Msg
view s model = 
  { title = "Settings"
  , body =
      E.column [ S.spacingMedium ]
        [ E.row [ S.spacingMedium ]
            [ Input.username []
                { onChange = UsernameChanged
                , text = model.newUsername
                , placeholder =
                    Just <| Input.placeholder [] <| E.text "new username ..."
                , label = S.labelSmall "change your username"
                }
            , case model.usernameAvailable of
               Just True -> S.text "available!"
               Just False -> S.text "not available :("
               Nothing    -> S.text ""
            ]
        , S.button "save!" <| Just SaveClicked
        , S.text <| model.feedback
        ]
  }

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of
    UsernameChanged new ->
      ( { model | newUsername = new, usernameAvailable = Nothing }
      , session
      , Api.getUsernameAvailability new GotUsernameAvailable
      )
    SaveClicked ->
      let
        cmd = 
          case session.userAndToken of
            Just uAndT ->
              let
                oldUser = uAndT.user
                newUser = { oldUser | username = model.newUsername }
              in
                Api.putUser uAndT newUser GotChangedUser
            Nothing ->
              Cmd.none
      in 
        ({ model | feedback = "Saving..." }, session, cmd)
    GotChangedUser result ->
      case result of
        Ok changedUAndT ->
          ( { model | feedback = "Saved!" }
          , { session | userAndToken = Just changedUAndT }
          , Session.store <| Just changedUAndT
          )
        Err _ ->
          ({ model | feedback = "error saving :(" }, session, Cmd.none)
    GotUsernameAvailable result ->
      case result of
        Ok bool ->
          ({ model | usernameAvailable = Just bool }, session, Cmd.none)
        Err _ ->
          (model, session, Cmd.none)
