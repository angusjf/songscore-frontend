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
  }

type Msg
  = UsernameChanged String
  | PasswordChanged String
  | LogInPressed
  | Completed (Result Http.Error Api.UserAndToken)

type alias Problem = String

init : Session.Data -> (Model, Session.Data, Cmd Msg)
init session =
  let
    model = 
      { username = ""
      , password = ""
      , problems = []
      }
  in
    (model, session, Cmd.none)

view : Model -> Page Msg
view model = 
  { title = "Log in"
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

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of
    UsernameChanged new -> ({ model | username = new }, session, Cmd.none)
    PasswordChanged new -> ({ model | password = new }, session, Cmd.none)
    LogInPressed -> 
      case (validate model) of
        Ok creds ->
            ({ model | problems = [] }, session, Api.postLogin creds Completed)
        Err problems -> ({ model | problems = problems }, session, Cmd.none)
    Completed result ->
      case result of
        Ok userAndToken ->
          let
            newSession = { session | userAndToken = Just userAndToken }
          in
            ( model
            , newSession
            , Cmd.batch
                [ Route.goTo session.key Route.Feed
                , Session.store newSession
                ]
            )
        Err _ ->
            ({model | password = "", problems = [ "error!" ] }, session, Cmd.none) -- TODO handle

validate : Model -> Result (List Problem) Credentials
validate model =
  Ok { username = model.username, password = model.password }
