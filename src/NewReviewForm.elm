module NewReviewForm exposing (..)

import Review exposing (..)
import Subject exposing (..)
import User exposing (..)

import Element exposing (Element, el, text, row, column, alignRight, fill, width, rgb255, spacing, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

type alias NewReviewForm msg =
  { text : Maybe String
  , stars : Maybe Int
  , subject : Maybe Subject
  , subjectQuery : Maybe String
  , onPress : msg
  , onChange : String -> msg
  , starsRadioChanged : Int -> msg
  , onSubjectQueryChanged : String -> msg
  }

viewNewReviewForm : NewReviewForm msg -> Element msg
viewNewReviewForm form =
  Element.column []
    [ viewNewReviewFormSubjectForm form
    , viewNewReviewFormStars form
    , viewNewReviewFormMultiline form
    , viewNewReviewFormSubmitButton form
    ]

viewNewReviewFormSubjectForm : NewReviewForm msg -> Element msg
viewNewReviewFormSubjectForm form =
  Input.text []
    { onChange = form.onSubjectQueryChanged
    , text = Maybe.withDefault "" form.subjectQuery
    , placeholder = Just (Input.placeholder [] (Element.text "search for a subect"))
    , label = Input.labelAbove [] (Element.text "Subject...")
    }

viewNewReviewFormStars : NewReviewForm msg -> Element msg
viewNewReviewFormStars form =
  Input.radio []
    { onChange = form.starsRadioChanged
    , selected = form.stars
    , label = Input.labelAbove [] (text "Stars")
    , options =
      [ Input.option 1 (text "1")
      , Input.option 2 (text "2")
      , Input.option 3 (text "3")
      , Input.option 4 (text "4")
      , Input.option 5 (text "5")
      ]
    }

viewNewReviewFormSubmitButton : NewReviewForm msg -> Element msg
viewNewReviewFormSubmitButton form =
  Input.button []
    { onPress = Just form.onPress
    , label = Element.text "Post!"
    }

viewNewReviewFormMultiline : NewReviewForm msg -> Element msg
viewNewReviewFormMultiline form =
  Input.multiline
    []
    { onChange = form.onChange
    , text = Maybe.withDefault "" form.text
    , placeholder = Just (Input.placeholder [] (Element.text "type your review here!"))
    , label = Input.labelAbove [] (Element.text "Review text")
    , spellcheck = True
    }

convertToNewReview : NewReviewForm msg -> User -> Maybe Review
convertToNewReview form user =
  case form.stars of
    Just stars ->
      case form.subject of
        Just subject -> Just
          { id = Nothing
          , text = form.text
          , stars = stars
          , user = user
          , subject = subject
          }
        Nothing -> Nothing
    Nothing -> Nothing

setReviewFormText : NewReviewForm msg -> String -> NewReviewForm msg
setReviewFormText form text = { form | text = Just text }

setReviewFormStars : NewReviewForm msg -> Int -> NewReviewForm msg
setReviewFormStars form n = { form | stars = Just n }

setReviewFormSubjectQuery : NewReviewForm msg -> String -> NewReviewForm msg
setReviewFormSubjectQuery form query = { form | subjectQuery = Just query, subject = Just {id = Nothing, image = Nothing, kind = Nothing, title = query} }
