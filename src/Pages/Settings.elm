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
import File
import Task
import File.Select as Select

type alias Model =
  { usernameAvailable : Maybe Bool
  , newUsername : String
  , feedback : String
  , profilePicture : String
  , oldUsername : String
  }

type Msg
  = UsernameChanged String
  | SaveClicked
  | GotChangedUser (Result Http.Error Api.UserAndToken)
  | GotUsernameAvailable (Result Http.Error Bool)
  | ProfilePicturePressed
  | OnImageSelected File.File
  | ImageDecoded String

init : Session.Data -> (Model, Session.Data, Cmd Msg)
init session =
  let
    oldUsername =
      Maybe.withDefault "not logged in :(" <|
        Maybe.map (\x -> x.user.username) session.userAndToken
    model =
      { usernameAvailable = Nothing
      , newUsername = oldUsername
      , feedback = ""
      , profilePicture =
          Maybe.withDefault "/assets/images/default-user.png" <|
            Maybe.andThen (\x -> x.user.image) session.userAndToken
      , oldUsername = oldUsername
      }
  in
    (model, session, Cmd.none)

view : Session.Data -> Model -> Page Msg
view session model = 
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
        , E.column [ S.spacingMedium ]
          [ E.image [ E.width (E.px 128) ]
              { src = model.profilePicture
              , description = "profile picture"
              }
          , S.button "upload new picture" <| Just ProfilePicturePressed
          ]
        , S.button "save!" <| Just SaveClicked
        , S.text <| model.feedback
        ]
  }

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of
    UsernameChanged new ->
      if new /= model.oldUsername && String.length new > 1 then
        ( { model | newUsername = new, usernameAvailable = Nothing }
        , session
        , Api.getUsernameAvailability new GotUsernameAvailable
        )
      else 
        ( { model | newUsername = new, usernameAvailable = Nothing }
        , session
        , Cmd.none
        )
    SaveClicked ->
      let
        cmd = 
          case session.userAndToken of
            Just uAndT ->
              let
                oldUser = uAndT.user
                newUser =
                  { oldUser
                    | username = model.newUsername
                    , image = Just model.profilePicture
                  }
              in
                Api.putUser uAndT newUser GotChangedUser
            Nothing ->
              Cmd.none
      in 
        ({ model | feedback = "Saving..." }, session, cmd)
    GotChangedUser result ->
      case result of
        Ok changedUAndT ->
          let
            newSession = { session | userAndToken = Just changedUAndT }
          in
            ( { model | feedback = "Saved!" }
            , newSession
            , Session.store newSession
            )
        Err _ ->
          ({ model | feedback = "error saving :(" }, session, Cmd.none)
    GotUsernameAvailable result ->
      case result of
        Ok bool ->
          ({ model | usernameAvailable = Just bool }, session, Cmd.none)
        Err _ ->
          ({ model | feedback = "error checking username :(" }, session, Cmd.none)
    ProfilePicturePressed ->
      (model, session, Select.file ["image/jpeg", "image/png"] OnImageSelected)
    OnImageSelected file ->
      (model, session, Task.perform ImageDecoded (File.toUrl file))
    ImageDecoded url ->
      ({ model | profilePicture = url }, session, Cmd.none)

