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

type alias Model = { page : Page, session : Session.Data }

type Page
  = NotFound
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

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  stepUrl url { page = NotFound, session = Session.fromNavKey key }

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          (model, Nav.pushUrl model.session.key (Url.toString url))
        Browser.External href ->
          (model, Nav.load href)
    UrlChanged url ->
      stepUrl url model
    RegisterMsg msg ->
      case model.page of
        Register innerModel ->
          stepRegister model <| Register.update msg innerModel model.session
        _ -> (model, Cmd.none)
    LoginMsg msg ->
      case model.page of
        Login innerModel ->
          stepLogin model <| Login.update msg innerModel model.session
        _ -> (model, Cmd.none)
    FeedMsg msg ->
      case model.page of
        Feed innerModel ->
          stepFeed model <| Feed.update msg innerModel model.session
        _ -> (model, Cmd.none)
    None -> (model, Cmd.none)
    UserMsg msg ->
      case model.page of
        User innerModel ->
          stepUser model <| User.update msg innerModel model.session
        _ -> (model, Cmd.none)
    ReviewMsg msg ->
      case model.page of
        Review innerModel ->
          stepReview model <| Review.update msg innerModel model.session
        _ -> (model, Cmd.none)
    NavbarMsg msg ->
      stepNavbar model <| Navbar.update msg model.session

stepUrl : Url.Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
  case (Route.fromUrl url) of
    Nothing -> ({ model | page = NotFound }, Cmd.none)
    Just Route.Register -> stepRegister model (Register.init model.session)
    Just Route.Feed -> stepFeed model (Feed.init model.session)
    Just Route.Login -> stepLogin model (Login.init model.session)
    Just Route.Root ->
      case model.session.userAndToken of
        Just _ -> stepFeed model (Feed.init model.session)
        Nothing -> stepLogin model (Login.init model.session)
    Just (Route.User username) ->
      stepUser model (User.init model.session username)
    Just (Route.Review username id) ->
      stepReview model (Review.init model.session username id)

stepRegister : Model -> (Register.Model, Session.Data, Cmd Register.Msg) -> (Model, Cmd Msg)
stepRegister model (registerModel, session, registerCmd) =
  ( { page = Register registerModel, session = session }
  , Cmd.map RegisterMsg registerCmd
  )

stepLogin : Model -> (Login.Model, Session.Data, Cmd Login.Msg) -> (Model, Cmd Msg)
stepLogin model (loginModel, session, loginCmd) =
  ( { page = Login loginModel, session = session }
  , Cmd.map LoginMsg loginCmd
  )

stepFeed : Model -> (Feed.Model, Session.Data, Cmd Feed.Msg) -> (Model, Cmd Msg)
stepFeed model (feedModel, session, feedCmd) =
  ( { page = Feed feedModel, session = session}
  , Cmd.map FeedMsg feedCmd
  )

stepUser : Model -> (User.Model, Session.Data, Cmd User.Msg) -> (Model, Cmd Msg)
stepUser model (userModel, session, userCmd) =
  ( { page = User userModel, session = session }
  , Cmd.map UserMsg userCmd
  )

stepReview : Model -> (Review.Model, Session.Data, Cmd Review.Msg) -> (Model, Cmd Msg)
stepReview model (reviewModel, session, reviewCmd) =
  ( { page = Review reviewModel, session = session }
  , Cmd.map ReviewMsg reviewCmd
  )

stepNavbar : Model -> (Session.Data, Cmd Msg) -> (Model, Cmd Msg)
stepNavbar model (session, cmd) = ({ model | session = session }, cmd)

view : Model -> Browser.Document Msg
view model =
  let
    { title, body } =
      case model.page of
        Register innerModel ->
          Page.map RegisterMsg <| Register.view innerModel
        Login innerModel ->
          Page.map LoginMsg <| Login.view innerModel
        Feed innerModel ->
          Page.map FeedMsg <| Feed.view model.session innerModel
        NotFound ->
          Page.map (always None) <| NotFound.view
        User innerModel ->
          Page.map UserMsg <| User.view model.session innerModel
        Review innerModel ->
          Page.map ReviewMsg <| Review.view model.session innerModel
    bar = Navbar.view NavbarMsg model.session
  in
    { title = "SongScore: " ++ title
    , body = [ S.skeleton bar body ]
    }

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
