
onEnter : msg -> Element.Attribute msg
onEnter msg =
  let 
   decoder = D.string |> D.andThen checkEnter
   checkEnter key = if key == "Enter"
                      then D.succeed msg
                      else D.fail "Not the enter key"
  in
    Element.htmlAttribute <|
      Html.Events.on "keyup" <|
        D.field "key" decoder
