module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url
import Html
import Pages.Register as Register
import Session
import Route

type alias Model =
  { key : Nav.Key
  , page : Page
  }

type Page
  = NotFound Session.Data
  | Register Register.Model

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | RegisterMsg Register.Msg

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
 stepUrl url
   { key = key
   , page = NotFound Session.empty
   }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          (model, Nav.pushUrl model.key (Url.toString url))
        Browser.External href ->
          (model, Nav.load href)
    UrlChanged url ->
      stepUrl url model
    RegisterMsg registerMsg ->
      case model.page of
        Register registerModel ->
            stepRegister model <| Register.update registerMsg registerModel
        _ ->
            (model, Cmd.none)

stepRegister : Model -> (Register.Model, Cmd Register.Msg) -> (Model, Cmd Msg)
stepRegister model (registerModel, registerCmd) =
  ( { model | page = Register registerModel }
  , Cmd.map RegisterMsg registerCmd
  )

view : Model -> Browser.Document Msg
view model =
  case model.page of
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

stepUrl : Url.Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
  let
    session = getSession model
    route = Route.fromUrl url
  in
    case route of
      Nothing -> ({ model | page = NotFound session }, Cmd.none)
      Just Route.Register -> stepRegister model (Register.init session)

getSession : Model -> Session.Data
getSession model =
  case model.page of 
    NotFound session -> session
    Register registerModel -> registerModel.session

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }
