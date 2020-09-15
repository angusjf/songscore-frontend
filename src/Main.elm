module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url
import Html
import Time
import Task
import Element
import Http
import Session
import Route
import Page
import Api exposing (UserAndToken)
import Styles as S
import Pages.Register as Register
import Pages.Login as Login
import Pages.Feed as Feed
import Pages.NotFound as NotFound
import Pages.User as User
import Pages.Review as Review
import Pages.Settings as Settings
import Pages.Notifications as Notifications
import Widgets.Navbar as Navbar

type alias Model =
  { page : Page
  , session : Session.Data
  , navbarModel : Navbar.Model
  }

type Page
  = NotFound
  | Register Register.Model
  | Feed Feed.Model
  | Login Login.Model
  | User User.Model
  | Review Review.Model
  | Settings Settings.Model
  | Notifications Notifications.Model

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | RegisterMsg Register.Msg
  | FeedMsg Feed.Msg
  | LoginMsg Login.Msg
  | UserMsg User.Msg
  | ReviewMsg Review.Msg
  | SettingsMsg Settings.Msg
  | NotificationsMsg Notifications.Msg
  | NavbarMsg Navbar.Msg
  | None
  | NewTime Time.Posix
  | GotNewNotifications (Result Http.Error Bool)

init : (Maybe UserAndToken, Int) -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init (uAndT, millis) url key =
  let
    (nav, navCmd) = Navbar.init uAndT
    initSession =
      { userAndToken = uAndT
      , key = key
      , currentTime = Time.millisToPosix millis
      , unreadNotifications = Nothing
      }
    start =
      { page = NotFound
      , session = initSession
      , navbarModel = nav
      }
    (model, cmd) = stepUrl url start
    cmds =
      case uAndT of
        Just ut ->
          [ cmd
          , Cmd.map NavbarMsg navCmd
          , Api.getNewNotifications ut GotNewNotifications
          , Task.perform NewTime Time.now
          ]
        Nothing ->
          [ cmd
          , Cmd.map NavbarMsg navCmd
          , Task.perform NewTime Time.now
          ]
  in
    (model, Cmd.batch cmds) 

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
    SettingsMsg msg ->
      case model.page of
        Settings innerModel ->
          stepSettings model <| Settings.update msg innerModel model.session
        _ -> (model, Cmd.none)
    NotificationsMsg msg ->
      case model.page of
        Notifications innerModel ->
          stepNotifications model <| Notifications.update msg innerModel model.session
        _ -> (model, Cmd.none)
    NavbarMsg msg ->
      stepNavbar model <| Navbar.update msg model.navbarModel model.session
    NewTime now ->
      let
        oldSession = model.session
        newSession = { oldSession | currentTime = now }
      in
        ({ model | session = newSession }, Cmd.none)
    GotNewNotifications result ->
      case result of
        Ok unread ->
          let
            oldSession = model.session
            newSession = { oldSession | unreadNotifications = Just unread }
          in
            ( { model | session = newSession }
            , Cmd.none
            )
        Err _ ->
          let
            oldSession = model.session
            loggedOut = { oldSession | userAndToken = Nothing }
          in
            ({ model | session = loggedOut }, Cmd.none)

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
    Just Route.Settings ->
      stepSettings model (Settings.init model.session)
    Just Route.Notifications ->
      stepNotifications model (Notifications.init model.session)

stepRegister : Model -> (Register.Model, Session.Data, Cmd Register.Msg) -> (Model, Cmd Msg)
stepRegister model (registerModel, session, registerCmd) =
  ( { model | page = Register registerModel, session = session }
  , Cmd.map RegisterMsg registerCmd
  )

stepLogin : Model -> (Login.Model, Session.Data, Cmd Login.Msg) -> (Model, Cmd Msg)
stepLogin model (loginModel, session, loginCmd) =
  ( { model | page = Login loginModel, session = session }
  , Cmd.map LoginMsg loginCmd
  )

stepFeed : Model -> (Feed.Model, Session.Data, Cmd Feed.Msg) -> (Model, Cmd Msg)
stepFeed model (feedModel, session, feedCmd) =
  ( { model | page = Feed feedModel, session = session}
  , Cmd.map FeedMsg feedCmd
  )

stepUser : Model -> (User.Model, Session.Data, Cmd User.Msg) -> (Model, Cmd Msg)
stepUser model (userModel, session, userCmd) =
  ( { model | page = User userModel, session = session }
  , Cmd.map UserMsg userCmd
  )

stepReview : Model -> (Review.Model, Session.Data, Cmd Review.Msg) -> (Model, Cmd Msg)
stepReview model (reviewModel, session, reviewCmd) =
  ( { model | page = Review reviewModel, session = session }
  , Cmd.map ReviewMsg reviewCmd
  )

stepSettings : Model -> (Settings.Model, Session.Data, Cmd Settings.Msg) -> (Model, Cmd Msg)
stepSettings model (settingsModel, session, settingsCmd) =
  ( { model | page = Settings settingsModel, session = session }
  , Cmd.map SettingsMsg settingsCmd
  )

stepNotifications : Model -> (Notifications.Model, Session.Data, Cmd
          Notifications.Msg) -> (Model, Cmd Msg)
stepNotifications model (notificationsModel, session, notificationsCmd) =
  ( { model | page = Notifications notificationsModel, session = session }
  , Cmd.map NotificationsMsg notificationsCmd
  )

stepNavbar : Model -> (Navbar.Model, Session.Data, Cmd Navbar.Msg) -> (Model, Cmd Msg)
stepNavbar model (navbarModel, session, navbarCmd) =
  ( { model | session = session, navbarModel = navbarModel }
  , Cmd.map NavbarMsg navbarCmd
  )

view : Model -> Browser.Document Msg
view model =
  let
    bar = Element.map NavbarMsg <|
            Navbar.view model.session model.navbarModel
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
        Settings innerModel ->
          Page.map SettingsMsg <| Settings.view model.session innerModel
        Notifications innerModel ->
          Page.map NotificationsMsg <| Notifications.view model.session innerModel
  in
    { title = "SongScore: " ++ title
    , body = [ S.skeleton bar body ]
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 3500 NewTime
    ]

main : Program (Maybe UserAndToken, Int) Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }
