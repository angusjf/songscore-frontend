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

update : Msg -> Session.Data -> Cmd msg
update msg session =
  case msg of
    OnLogoClicked ->
      Route.goTo session.key Route.Feed
    OnSignupClicked ->
      Route.goTo session.key Route.Register
    OnLoginClicked ->
      Route.goTo session.key Route.Login
    OnUserClicked ->
      case session.userAndToken of
        Just uAndT -> Route.goTo session.key (Route.User uAndT.user.username)
        Nothing -> Cmd.none

view : (Msg -> msg) -> Maybe User -> E.Element msg
view toOuter maybeUser =
  E.row
    [ E.height E.shrink
    , E.width E.fill
    , E.spaceEvenly
    , Background.color S.red
    , S.paddingMixedMedium
    ]
    [ S.buttonAlt "Home" <| Just <| toOuter OnLogoClicked
    , E.row [ E.alignRight , S.spacingMedium ] <|
        case maybeUser of
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
            , S.buttonAlt "Log out" <| Nothing
            ]
          Nothing ->
              [ S.buttonAlt "Log in" <| Just <| toOuter OnLoginClicked
              , S.buttonAlt "Sign Up" <| Just <| toOuter OnSignupClicked
              ]
    ]
