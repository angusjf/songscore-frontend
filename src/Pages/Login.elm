module Pages.Login exposing (..)

import Element as E
import Page exposing (Page)
import Element.Input as Input
import Browser
import Api exposing (Credentials, postUser)
import Http
import User
import Session
import Route
import Styles as S

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
      E.column [ S.spacingMedium ]
        [ E.text ""
        , Input.username []
            { onChange = UsernameChanged
            , text = model.username
            , placeholder = Just (Input.placeholder [] (E.text "username"))
            , label = S.labelSmall "Username"
            }
        , Input.currentPassword []
            { onChange = PasswordChanged
            , text = model.password
            , placeholder = Just (Input.placeholder [] (E.text "password"))
            , label = S.labelSmall "Password"
            , show = False
            }
        , S.button "Log in" (Just LogInPressed)
        , E.column [] <|
            List.map E.text model.problems
        ]
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UsernameChanged new -> ({ model | username = new }, Cmd.none)
    PasswordChanged new -> ({ model | password = new }, Cmd.none)
    LogInPressed -> 
      case (validate model) of
        Ok creds ->
            ({ model | problems = [] }, Api.postLogin creds Completed)
        Err problems -> ({ model | problems = problems }, Cmd.none)
    Completed result ->
      case (Debug.log "got result" result) of
        Ok userAndToken ->
          let
            oldSession = model.session
            newSession = { oldSession | userAndToken = Just userAndToken }
            newModel = { model | session = newSession }
          in
            (newModel, Route.goTo model.session.key Route.Feed)
        Err _ ->
            ({model | password = "", problems = [ "error!" ] }, Cmd.none) -- TODO handle

validate : Model -> Result (List Problem) Credentials
validate model =
  Ok { username = model.username, password = model.password }
