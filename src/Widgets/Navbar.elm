module Widgets.Navbar exposing (..)

import Element as E
import Element.Background as Background
import Element.Input as Input
import User exposing (User)
import Styles as S 
import Element.Font as Font
import Route
import Session

type Msg
 = OnLogoClicked
 | OnLoginClicked
 | OnSignupClicked
 | OnUserClicked
 | OnLogoutClicked

update : Msg -> Session.Data -> (Session.Data, Cmd msg)
update msg session =
  case msg of
    OnLogoClicked ->
      (session, Route.goTo session.key Route.Feed)
    OnSignupClicked ->
      (session, Route.goTo session.key Route.Register)
    OnLoginClicked ->
      (session, Route.goTo session.key Route.Login)
    OnUserClicked ->
      case session.userAndToken of
        Just uAndT ->
          (session, Route.goTo session.key (Route.User uAndT.user.username))
        Nothing ->
          (session, Cmd.none)
    OnLogoutClicked ->
      ({ session | userAndToken = Nothing }, Route.goTo session.key Route.Root)

view : (Msg -> msg) -> Session.Data -> E.Element msg
view toOuter session =
  E.row
    [ E.height E.shrink
    , E.width E.fill
    , E.spaceEvenly
    , Background.color S.red
    , S.paddingMixedMedium
    ]
    [ S.buttonAlt "Home" <| Just <| toOuter OnLogoClicked
    , E.row [ E.alignRight , S.spacingMedium ] <|
        case Maybe.map .user session.userAndToken of
          Just user ->
            [ Input.button
              [ E.alignRight
              , S.spacingMedium
              , Background.color S.white
              , Font.color S.red
              , Font.bold
              , S.paddingSmall
              , S.roundedSmall
              ]
              { onPress = Just <| toOuter OnUserClicked
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
            , S.buttonAlt "Log out" <| Just <| toOuter OnLogoutClicked
            ]
          Nothing ->
            [ S.buttonAlt "Log in" <| Just <| toOuter OnLoginClicked
            , S.buttonAlt "Sign Up" <| Just <| toOuter OnSignupClicked
            ]
    ]
