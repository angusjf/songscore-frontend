module Widgets.NewReviewForm exposing (..)

import Review exposing (..)
import Subject exposing (..)
import User exposing (..)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Styles as S

type alias NewReviewForm msg =
  { text : Maybe String
  , stars : Maybe Int
  , subject : Maybe Subject
  , subjectQuery : Maybe String
  , onPress : msg
  , onChange : String -> msg
  , onStarsChanged : Int -> msg
  , onSubjectQueryChanged : String -> msg
  }

view : NewReviewForm msg -> Element msg
view form =
  E.column [S.roundedSmall, Border.color S.red, S.borderSmall, S.paddingSmall]
    [ E.text "Write a Review!"
    , viewSubjectForm form
    , viewStars form
    , viewMultiline form
    , viewSubmitButton form
    ]

viewSubjectForm : NewReviewForm msg -> Element msg
viewSubjectForm form =
  Input.text []
    { onChange = form.onSubjectQueryChanged
    , text = Maybe.withDefault "" form.subjectQuery
    , placeholder = Just (Input.placeholder [] (E.text "search for a subject"))
    , label = Input.labelAbove [] (E.text "Subject...")
    }

{-
viewStars : NewReviewForm msg -> Element msg
viewStars form =
  Input.radio []
    { onChange = form.onStarsChanged
    , selected = form.stars
    , label = Input.labelAbove [] (E.text "Stars")
    , options =
      [ Input.option 1 (text "1")
      , Input.option 2 (text "2")
      , Input.option 3 (text "3")
      , Input.option 4 (text "4")
      , Input.option 5 (text "5")
      ]
    }
-}

viewStars : NewReviewForm msg -> Element msg
viewStars form = 
  case form.stars of 
    Just n ->
      E.row [] <| 
        List.map2 (\f x -> f (form.onStarsChanged x))
          ((List.repeat n redStar) ++ (List.repeat (5 - n) greyStar)) [1, 2, 3, 4, 5]
    Nothing ->
      E.row [] <| 
        List.map2 (\f x -> f (form.onStarsChanged x))
          (List.repeat 5 greyStar) [1, 2, 3, 4, 5]

redStar : msg -> Element msg
redStar msg =
  Input.button []
    { onPress = Just msg
    , label = 
        E.image []
          { src = "/assets/images/red-star.png"
          , description = "red star" }
    }

greyStar : msg -> Element msg
greyStar msg =
  Input.button []
    { onPress = Just msg
    , label = 
        E.image []
          { src = "/assets/images/grey-star.png"
          , description = "grey star" }
    }

viewSubmitButton : NewReviewForm msg -> Element msg
viewSubmitButton form =
  Input.button []
    { onPress = Just form.onPress
    , label = E.text "Post!"
    }

viewMultiline : NewReviewForm msg -> Element msg
viewMultiline form =
  Input.multiline
    []
    { onChange = form.onChange
    , text = Maybe.withDefault "" form.text
    , placeholder = Just (Input.placeholder [] (E.text "type your review here!"))
    , label = Input.labelAbove [] (E.text "Review text")
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

setText : NewReviewForm msg -> String -> NewReviewForm msg
setText form text = { form | text = Just text }

setStars : NewReviewForm msg -> Int -> NewReviewForm msg
setStars form n = { form | stars = Just n }

setSubjectQuery : NewReviewForm msg -> String -> NewReviewForm msg
setSubjectQuery form query =
    { form | subjectQuery = Just query,
             subject = Just {id = Nothing, image = Nothing, kind = Nothing, artist = Nothing, title = query}
    }
