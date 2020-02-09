module Pages.Register exposing (..)

import Element exposing (column, text)
import Page exposing (Page)
import Element.Input as Input
import Browser
import Api exposing (NewUser, postUser)
import Http
import User
import Session
import Route
import Styles as S
import File
import File.Select as Select
import Task

type alias Model =
  { username : String
  , password : String
  , passwordRepeat : String
  , problems : List Problem
  , session : Session.Data
  , profilePicture : Maybe String
  }

type Msg
  = UsernameChanged String
  | PasswordChanged String
  | PasswordRepeatChanged String
  | SignUpPressed
  | Completed (Result Http.Error Api.UserAndToken)
  | ProfilePicturePressed
  | OnImageSelected File.File
  | ImageDecoded String

type alias Problem = String

init : Session.Data -> (Model, Cmd Msg)
init session =
  let
    model = 
      { username = ""
      , password = ""
      , passwordRepeat = ""
      , problems = []
      , session = session
      , profilePicture = Nothing
      }
  in
    (model, Cmd.none)

view : Model -> Page Msg
view model = 
  { title = "Register"
  , body =
      column [ S.spacingMedium ]
        [ text ""
        , Input.username []
            { onChange = UsernameChanged
            , text = model.username
            , placeholder = Just (Input.placeholder [] (text "username"))
            , label = S.labelSmall "Username"
            }
        , Input.newPassword []
            { onChange = PasswordChanged
            , text = model.password
            , placeholder = Just (Input.placeholder [] (Element.text "password"))
            , label = S.labelSmall "Password"
            , show = False
            }
        , Input.newPassword []
            { onChange = PasswordRepeatChanged
            , text = model.passwordRepeat
            , placeholder = Just (Input.placeholder [] (Element.text "repeat password"))
            , label = S.labelSmall "Repeat password please"
            , show = False
            }
        , S.button "Profile picture!" (Just ProfilePicturePressed)
        , S.button "Sign Up" (Just SignUpPressed)
        , column [] <|
            List.map text model.problems
        ]
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UsernameChanged new -> ({ model | username = new }, Cmd.none)
    PasswordChanged new -> ({ model | password = new }, Cmd.none)
    PasswordRepeatChanged new -> ({ model | passwordRepeat = new }, Cmd.none)
    SignUpPressed -> 
      case validate model of
        Ok newUser ->
            ({ model | problems = [] }, Api.postUser newUser Completed)
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
    ProfilePicturePressed ->
      (model, Select.file ["image/jpeg", "image/png"] OnImageSelected)
    OnImageSelected file ->
      (model, Task.perform ImageDecoded (File.toUrl file))
    ImageDecoded url ->
      ({ model | profilePicture = Just url }, Cmd.none)

validate : Model -> Result (List Problem) NewUser
validate model =
  if model.password == model.passwordRepeat
    then Ok
      { username = model.username
      , password = model.password
      , image = Maybe.withDefault "" model.profilePicture
      }
    else Err [ "Passwords Should Match" ]
