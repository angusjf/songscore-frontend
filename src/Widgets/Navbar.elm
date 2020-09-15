module Widgets.Navbar exposing (..)

import Element as E
import Element.Background as Background
import Element.Input as Input
import User exposing (User)
import Styles as S 
import Element.Font as Font
import Route
import Session
import Api exposing (UserAndToken)
import Http

type Msg
 = OnLogoClicked
 | OnLoginClicked
 | OnSignupClicked
 | OnUserClicked
 | OnLogoutClicked
 | OnSettingsClicked
 | OnNotificationsClicked

type alias Model = ()

init : Maybe UserAndToken -> (Model, Cmd Msg)
init userAndToken = 
  let
    model = ()
  in
    (model, Cmd.none)

update : Msg -> Model -> Session.Data -> (Model, Session.Data, Cmd Msg)
update msg model session =
  case msg of
    OnLogoClicked ->
      (model, session, Route.goTo session.key Route.Feed)
    OnSignupClicked ->
      (model, session, Route.goTo session.key Route.Register)
    OnLoginClicked ->
      (model, session, Route.goTo session.key Route.Login)
    OnUserClicked ->
      case session.userAndToken of
        Just uAndT ->
          (model, session, Route.goTo session.key (Route.User uAndT.user.username))
        Nothing ->
          (model, session, Cmd.none)
    OnLogoutClicked ->
      ( model
      , { session | userAndToken = Nothing }
      , Cmd.batch
          [ Route.goTo session.key Route.Root
          , Session.store { session | userAndToken = Nothing }
          ]
      )
    OnSettingsClicked ->
      (model, session, Route.goTo session.key Route.Settings)
    OnNotificationsClicked ->
      ( model
      , session
      , Route.goTo session.key Route.Notifications
      )

view : Session.Data -> Model -> E.Element Msg
view session model =
  let
    home = S.buttonIcon "fas fa-star" <| Just <| OnLogoClicked
    notifs =
        E.row
            [S.spacingMedium] <|
            [ S.buttonIcon "fas fa-bell" (Just OnNotificationsClicked)
            ] ++
            ( if Maybe.withDefault False session.unreadNotifications
                then [S.textAlt "(new)"]
                else [] )
          
    profile user =
      Input.button
        [ E.alignRight
        , S.spacingMedium
        , Background.color S.white
        , Font.color S.red
        , Font.bold
        , S.paddingSmall
        , S.roundedSmall
        ]
        { onPress = Just <| OnUserClicked
        , label = 
            case user.image of
              Just image ->
                E.row [S.spacingSmall]
                  [ E.image [ E.width (E.px 32) ]
                      { src = image
                      , description = "profile picture"
                      }
                  , S.text user.username
                  ]
              Nothing -> S.text user.username
        }
    settings = S.buttonIcon "fas fa-cog" <| Just <| OnSettingsClicked
    logout = S.buttonAlt "Log out" <| Just <| OnLogoutClicked
    login = S.buttonAlt "Log in" <| Just <| OnLoginClicked
    signup = S.buttonAlt "Sign Up" <| Just <| OnSignupClicked
  in
    E.row
      [ E.height E.shrink
      , E.width E.fill
      , E.centerX
      , E.spaceEvenly
      , Background.color S.red
      , S.paddingMixedMedium
      , S.spacingMedium
      , S.lightShadow
      ] <|
      case Maybe.map .user session.userAndToken of
        Just user ->
          [ home
          , notifs
          , E.row [ E.alignRight, S.spacingMedium ]
              [ profile user
              , settings
              , logout
              ]
          ]
        Nothing ->
          [ home
          , E.row [ E.alignRight, S.spacingMedium ]
              [ login
              , signup
              ]
          ]
