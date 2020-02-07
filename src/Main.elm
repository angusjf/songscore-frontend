module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url
import Html
import Pages.Register as Register
import Pages.Feed as Feed
import Session
import Route
import Element
import Page

type Page
  = NotFound Session.Data
  | Register Register.Model
  | Feed Feed.Model

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | RegisterMsg Register.Msg
  | FeedMsg Feed.Msg

init : () -> Url.Url -> Nav.Key -> (Page, Cmd Msg)
init _ url key = stepUrl url (NotFound (Session.fromNavKey key))

update : Msg -> Page -> (Page, Cmd Msg)
update message page =
  case message of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          (page, Nav.pushUrl (getSession page).key (Url.toString url))
        Browser.External href ->
          (page, Nav.load href)
    UrlChanged url ->
      stepUrl url page
    RegisterMsg msg ->
      case page of
        Register model ->
            stepRegister page <| Register.update msg model
        _ ->
            (page, Cmd.none)
    FeedMsg msg ->
      case page of
        Feed model ->
            stepFeed page <| Feed.update msg model
        _ ->
            (page, Cmd.none)

stepRegister : Page -> (Register.Model, Cmd Register.Msg) -> (Page, Cmd Msg)
stepRegister model (registerModel, registerCmd) =
  ( Register registerModel
  , Cmd.map RegisterMsg registerCmd
  )

stepFeed : Page -> (Feed.Model, Cmd Feed.Msg) -> (Page, Cmd Msg)
stepFeed model (feedModel, feedCmd) =
  ( Feed feedModel
  , Cmd.map FeedMsg feedCmd
  )

view : Page -> Browser.Document Msg
view page =
  let
    { title, body } =
      case page of
        Register model -> Page.map RegisterMsg (Register.view model)
        Feed model -> Page.map FeedMsg (Feed.view model)
        NotFound session ->
          { title = "Not Found"
          , body = Element.text "Not Found!"
          }
  in
    { title = title
    , body = [ Element.layout [] body ]
    }

stepUrl : Url.Url -> Page -> (Page, Cmd Msg)
stepUrl url page =
  let
    session = getSession page
    route = Route.fromUrl url
  in
    case route of
      Nothing -> (NotFound session, Cmd.none)
      Just Route.Register -> stepRegister page (Register.init session)
      Just Route.Feed -> stepFeed page (Feed.init session)

getSession : Page -> Session.Data
getSession page =
  case page of 
    NotFound session -> session
    Register model -> model.session
    Feed model -> model.session

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
