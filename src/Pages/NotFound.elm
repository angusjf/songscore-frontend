module Pages.NotFound exposing (..)

import Page exposing (Page)

import Styles as S

view : Page ()
view =
  { title = "Not Found"
  , body = S.text "Not Found!"
  }
