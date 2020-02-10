module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url
import Html
import Pages.Register as Register
import Pages.Login as Login
import Pages.Feed as Feed
import Pages.NotFound as NotFound
import Pages.User as User
import Pages.Review as Review
import Session
import Route
import Page
import Widgets.Navbar as Navbar
import Styles as S exposing (skeleton)

type Page
  = NotFound Session.Data
  | Register Register.Model
  | Feed Feed.Model
  | Login Login.Model
  | User User.Model
  | Review Review.Model

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | RegisterMsg Register.Msg
  | FeedMsg Feed.Msg
  | LoginMsg Login.Msg
  | UserMsg User.Msg
  | ReviewMsg Review.Msg
  | NavbarMsg Navbar.Msg
  | None

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
        _ -> (page, Cmd.none)
    LoginMsg msg ->
      case page of
        Login model ->
            stepLogin page <| Login.update msg model
        _ -> (page, Cmd.none)
    FeedMsg msg ->
      case page of
        Feed model ->
          stepFeed page <| Feed.update msg model
        _ -> (page, Cmd.none)
    None -> (page, Cmd.none)
    UserMsg msg ->
      case page of
        User model ->
          stepUser page <| User.update msg model
        _ -> (page, Cmd.none)
    ReviewMsg msg ->
      case page of
        Review model ->
          stepReview page <| Review.update msg model
        _ -> (page, Cmd.none)
    NavbarMsg msg ->
      (page, Navbar.update msg (getSession page))

stepRegister : Page -> (Register.Model, Cmd Register.Msg) -> (Page, Cmd Msg)
stepRegister model (registerModel, registerCmd) =
  ( Register registerModel
  , Cmd.map RegisterMsg registerCmd
  )

stepLogin : Page -> (Login.Model, Cmd Login.Msg) -> (Page, Cmd Msg)
stepLogin model (loginModel, loginCmd) =
  ( Login loginModel
  , Cmd.map LoginMsg loginCmd
  )

stepFeed : Page -> (Feed.Model, Cmd Feed.Msg) -> (Page, Cmd Msg)
stepFeed model (feedModel, feedCmd) =
  ( Feed feedModel
  , Cmd.map FeedMsg feedCmd
  )

stepUser : Page -> (User.Model, Cmd User.Msg) -> (Page, Cmd Msg)
stepUser model (userModel, userCmd) =
  ( User userModel
  , Cmd.map UserMsg userCmd
  )

stepReview : Page -> (Review.Model, Cmd Review.Msg) -> (Page, Cmd Msg)
stepReview model (reviewModel, reviewCmd) =
  ( Review reviewModel
  , Cmd.map ReviewMsg reviewCmd
  )

view : Page -> Browser.Document Msg
view page =
  let
    { title, body } =
      case page of
        Register model -> Page.map RegisterMsg (Register.view model)
        Login model -> Page.map LoginMsg (Login.view model)
        Feed model -> Page.map FeedMsg (Feed.view model)
        NotFound session -> Page.map (\_ -> None) NotFound.view
        User model -> Page.map UserMsg (User.view model)
        Review model -> Page.map ReviewMsg (Review.view model)
    bar = Navbar.view NavbarMsg (Maybe.map .user (getSession page).userAndToken)
  in
    { title = "SongScore: " ++ title
    , body = [ S.skeleton bar body ]
    }

stepUrl : Url.Url -> Page -> (Page, Cmd Msg)
stepUrl url page =
  let
    session = getSession page
  in
    case (Route.fromUrl url) of
      Nothing -> (NotFound session, Cmd.none)
      Just Route.Register -> stepRegister page (Register.init session)
      Just Route.Feed -> stepFeed page (Feed.init session)
      Just Route.Login -> stepLogin page (Login.init session)
      Just Route.Root ->
        case session.userAndToken of
          Just _ -> stepFeed page (Feed.init session)
          Nothing -> stepLogin page (Login.init session)
      Just (Route.User username) ->
        stepUser page (User.init session username)
      Just (Route.Review username id) ->
        stepReview page (Review.init session username id)

getSession : Page -> Session.Data
getSession page =
  case page of 
    NotFound session -> session
    Register model -> model.session
    Login model -> model.session
    Feed model -> model.session
    User model -> model.session
    Review model -> model.session

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
