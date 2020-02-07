module Widgets.Navbar exposing (..)

import Element exposing (Element, row)
import Element.Input as Input
import User exposing (User)

type alias NavBar msg =
  { loggedIn : Bool
  , onLoginClicked : Maybe msg
  , onSignupClicked : Maybe msg
  , onUserClicked : Maybe msg
  , onLogoClicked : Maybe msg
  , currentUser : Maybe User
  }

view : NavBar msg -> Element msg
view navbar =
  row []
    [ Input.button []
        { onPress = navbar.onLogoClicked
        , label = Element.text "home"
        }
    , Input.button []
        { onPress = navbar.onLoginClicked
        , label = Element.text "Log in"
        }
    , Input.button []
        { onPress = navbar.onSignupClicked
        , label = Element.text "Sign up"
        }
    ]
