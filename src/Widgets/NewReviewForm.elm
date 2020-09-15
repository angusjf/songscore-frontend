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
import Http
import Api
import Time

type Msg
  = OnTextChanged String
  | OnStarsChanged Int
  | OnSubjectQueryChanged String
  | GotResults (Result Http.Error (List Api.SubjectResult))
  | OnResultClicked Api.SubjectResult
  | OnSubjectClear

type alias Form msg =
  { text : Maybe String
  , stars : Maybe Int
  , subject : Maybe Subject
  , subjectQuery : String
  , results : List Api.SubjectResult
  , onPress : msg
  , toOuterMsg : Msg -> msg
  }

resultsLimit = 4
minSearchLength = 2

init : (Msg -> msg) -> msg -> Form msg
init toOuterMsg onPress =
  { text = Nothing
  , stars = Nothing
  , subject = Nothing
  , subjectQuery = ""
  , results = []
  , onPress = onPress
  , toOuterMsg = toOuterMsg
  }

update : Msg -> Form msg -> (Form msg, Cmd msg)
update msg model =
  case msg of
    OnTextChanged new ->
      ({ model | text = Just new }, Cmd.none)
    OnStarsChanged n ->
      ({ model | stars = Just n }, Cmd.none)
    OnSubjectQueryChanged new ->
          if String.length new < minSearchLength
            then ( { model
                     | subjectQuery = new
                     , results = []
                   }
                 , Cmd.none
                 )
            else ( { model | subjectQuery = new }
                 , Api.searchSubjects new resultsLimit <|
                         \x -> model.toOuterMsg (GotResults x)
                 )
    GotResults res ->
      case res of
        Ok results ->
          ( { model
              | results = List.take resultsLimit results
            }
          , Cmd.none
          )
        Err e ->
          Debug.todo <| Debug.toString e
    OnResultClicked track ->
      ({ model
         | subject = Just (resultToSubject track)
         , results = []
         , subjectQuery = ""
       }
      , Cmd.none
      )
    OnSubjectClear ->
      ({ model | subject = Nothing }
      , Cmd.none
      )

resultToSubject : Api.SubjectResult -> Subject
resultToSubject result =
  { id = Nothing
  , image = result.imageUrl
  , kind = 
      if result.kind == "Album" then
          Album
      else
          Song
  , title = result.name
  , artist = result.artist
  , spotifyId = result.spotifyId
  }

view : Form msg -> Element msg
view form =
  E.column
    [ S.roundedSmall
    , Border.color S.red
    , S.borderSmall
    , S.paddingSmall
    , S.spacingSmall
    , E.width (E.px 350)
    ]
    [ E.text "Write a Review!"
    , viewSubjectForm form
    , viewStars form
    , viewMultiline form
    , viewSubmitButton form
    ]

viewSubjectForm : Form msg -> Element msg
viewSubjectForm form =
  let
    searchBox =
      Input.text [E.width E.fill]
        { onChange = \s -> form.toOuterMsg (OnSubjectQueryChanged s)
        , text = form.subjectQuery
        , placeholder = Just (Input.placeholder [] (E.text "search for a subject"))
        , label = Input.labelHidden "Subject"
        }
    results =
      E.column [S.paddingSmall, E.width E.fill, E.alignTop] <|
               List.map (viewResult form.toOuterMsg) form.results
  in
    case form.subject of
      Just subject ->
        E.column [S.paddingSmall, E.width E.fill] <|
          [ S.subjectResult
              { artist = subject.artist
              , title = subject.title
              , image = subject.image
              }
          , S.button "clear" <| Just <| form.toOuterMsg OnSubjectClear
          ]
      Nothing ->
        E.column [S.paddingSmall, E.width E.fill] <|
          if List.isEmpty form.results then
              [searchBox]
          else
              [searchBox, results]

viewResult : (Msg -> msg) -> Api.SubjectResult -> Element msg
viewResult toOuterMsg result =
  Input.button [E.width E.fill]
    { onPress = Just <| toOuterMsg (OnResultClicked result)
    , label =
        E.row [ E.width E.fill, S.spacingSmall, S.paddingSmall ]
          [ E.image S.squareTiny
            { src = result.imageUrlSmall
            , description = ""
            }
          , E.column
              [ E.width E.fill ]
              [ E.paragraph [] [ S.text result.name ]
              , E.paragraph [] [ S.text result.artist ]
              ]
          ]
    }

viewStars : Form msg -> Element msg
viewStars form =
  case form.stars of
    Just n ->
      E.row [ E.width E.fill ] <|
        List.map2 (\f x -> f (form.toOuterMsg (OnStarsChanged x)))
          ((List.repeat n redStar) ++ (List.repeat (5 - n) greyStar)) [1, 2, 3, 4, 5]
    Nothing ->
      E.row [ E.width E.fill ] <|
        List.map2 (\f x -> f (form.toOuterMsg (OnStarsChanged x)))
          (List.repeat 5 greyStar) [1, 2, 3, 4, 5]

redStar : msg -> Element msg
redStar msg =
  Input.button [ E.width E.fill ]
    { onPress = Just msg
    , label =
        E.image [ E.width E.fill, E.height E.fill ]
          { src = "/assets/images/red-star.png"
          , description = "red star" }
    }

greyStar : msg -> Element msg
greyStar msg =
  Input.button [ E.width E.fill ]
    { onPress = Just msg
    , label =
        E.image [ E.width E.fill, E.height E.fill ]
          { src = "/assets/images/grey-star.png"
          , description = "grey star" }
    }

viewSubmitButton : Form msg -> Element msg
viewSubmitButton form = S.button "Post!" <| Just form.onPress

viewMultiline : Form msg -> Element msg
viewMultiline form = E.el [ S.paddingSmall, E.width E.fill ] <|
  Input.multiline
    [E.width E.fill]
    { onChange = \s -> form.toOuterMsg (OnTextChanged s)
    , text = Maybe.withDefault "" form.text
    , placeholder = Just (Input.placeholder [] (E.text "type your review here!"))
    , label = Input.labelHidden "Review text"
    , spellcheck = True
    }

convertToReview : Time.Posix -> Form msg -> User -> Maybe Review
convertToReview now form user =
  case (form.stars, form.subject) of
    (Just stars, Just subject) ->
      Just
        { id = Nothing
        , text = form.text
        , stars = stars
        , user = user
        , subject = subject
        , comments = []
        , likes = []
        , dislikes = []
        , insertedAt = now
        }
    (_, _) -> Nothing
