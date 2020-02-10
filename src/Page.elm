module Page exposing (..)

import Element exposing (Element)

type alias Page msg = { title: String, body: Element msg }

map : (msg -> msg1) -> Page msg -> Page msg1
map converter page =
  { title = page.title
  , body = Element.map converter page.body
  }
