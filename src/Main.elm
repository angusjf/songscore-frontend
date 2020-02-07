module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url
import Html
import Pages.Register as Register
import Session
import Route

type Page
  = NotFound Session.Data
  | Register Register.Model

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | RegisterMsg Register.Msg

init : () -> Url.Url -> Nav.Key -> (Page, Cmd Msg)
init _ url key = stepUrl url (NotFound (Session.fromNavKey key))

update : Msg -> Page -> (Page, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          (model, Nav.pushUrl (getSession model).key (Url.toString url))
        Browser.External href ->
          (model, Nav.load href)
    UrlChanged url ->
      stepUrl url model
    RegisterMsg registerMsg ->
      case model of
        Register registerModel ->
            stepRegister model <| Register.update registerMsg registerModel
        _ ->
            (model, Cmd.none)

stepRegister : Page -> (Register.Model, Cmd Register.Msg) -> (Page, Cmd Msg)
stepRegister model (registerModel, registerCmd) =
  ( Register registerModel
  , Cmd.map RegisterMsg registerCmd
  )

view : Page -> Browser.Document Msg
view model =
  case model of
    NotFound session ->
      { title = "Not Found"
      , body = []
      }
    Register registerModel ->
      { title = "Register"
      , body =
          [ Html.map RegisterMsg (Register.view registerModel)
          ]
      }

stepUrl : Url.Url -> Page -> (Page, Cmd Msg)
stepUrl url model =
  let
    session = getSession model
    route = Route.fromUrl url
  in
    case route of
      Nothing -> (NotFound session, Cmd.none)
      Just Route.Register -> stepRegister model (Register.init session)

getSession : Page -> Session.Data
getSession model =
  case model of 
    NotFound session -> session
    Register registerModel -> registerModel.session

main : Program () Page Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }
