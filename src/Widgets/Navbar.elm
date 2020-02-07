module Widgets.Navbar exposing (..)

import Element as E
import Element.Background as Background
import Element.Input as Input
import User exposing (User)
import Styles as S 

type alias NavBar msg =
  { loggedIn : Bool
  , onLoginClicked : Maybe msg
  , onSignupClicked : Maybe msg
  , onUserClicked : Maybe msg
  , onLogoClicked : Maybe msg
  , currentUser : Maybe User
  }

view : NavBar msg -> E.Element msg
view navbar =
  E.row
    [ E.height E.shrink
    , E.width E.fill
    , E.spaceEvenly
    , Background.color S.red
    , S.paddingMedium
    ]
    [ S.buttonAlt "~" navbar.onLogoClicked
    , E.row [ E.alignRight , S.spacingMedium ]
        [ S.buttonAlt "Log in" navbar.onLoginClicked
        , S.buttonAlt "Sign Up" navbar.onSignupClicked
        ]
    ]
