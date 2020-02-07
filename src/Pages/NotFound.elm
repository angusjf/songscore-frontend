module Pages.NotFound exposing (..)

import Page exposing (Page)

import Element exposing (column)

view : Page ()
view =
  { title = "Not Found"
  , body =
      column []
        [ Element.text "Not Found!"
        --, Element.link 
        ]
  }
