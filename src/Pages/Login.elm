module Pages.Login exposing (..)

import Element exposing (column, text)
import Page exposing (Page)
import Element.Input as Input
import Browser
import Api exposing (Credentials, postUser)
import Http
import User
import Session
import Route

type alias Model =
  { username : String
  , password : String
  , problems : List Problem
  , session : Session.Data
  }

type Msg
  = UsernameChanged String
  | PasswordChanged String
  | LogInPressed
  | Completed (Result Http.Error Api.UserAndToken)

type alias Problem = String

init : Session.Data -> (Model, Cmd Msg)
init session =
  let
    model = 
      { username = ""
      , password = ""
      , problems = []
      , session = session
      }
  in
    (model, Cmd.none)

view : Model -> Page Msg
view model = 
  { title = "Register"
  , body =
      column []
        [ text ""
        , Input.username []
            { onChange = UsernameChanged
            , text = model.username
            , placeholder = Just (Input.placeholder [] (text "username"))
            , label = Input.labelAbove [] (text "Username")
            }
        , Input.newPassword []
            { onChange = PasswordChanged
            , text = model.password
            , placeholder = Just (Input.placeholder [] (Element.text "password"))
            , label = Input.labelAbove [] (text "Password")
            , show = False
            }
        , Input.button []
          { onPress = Just LogInPressed
          , label = text "Log in"
          }
        , column [] <|
            List.map text model.problems
        ]
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UsernameChanged new -> ({ model | username = new }, Cmd.none)
    PasswordChanged new -> ({ model | password = new }, Cmd.none)
    LogInPressed -> 
      case validate model of
        Ok creds ->
            ({ model | problems = [] }, Api.postUser creds Completed)
        Err problems -> ({ model | problems = problems }, Cmd.none)
    Completed result ->
      case result of
        Ok userAndToken ->
          let
            oldSession = model.session
            newSession = { oldSession | userAndToken = Just userAndToken }
            newModel = { model | session = newSession }
          in
            (newModel, Route.goTo model.session.key Route.Feed)
        Err _ ->
            (model, Cmd.none) -- TODO handle

validate : Model -> Result (List Problem) Credentials
validate model =
  Ok { username = model.username, password = model.password }
