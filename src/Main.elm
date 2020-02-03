module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element, el, text, row, column, spacing, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Maybe exposing (Maybe)
import Json.Decode as D
import Json.Encode as E
import List exposing (map)
import Debug exposing (log)
import Review exposing (..)
import User exposing (..)
import Subject exposing (..)
import NewReviewForm exposing (..)

type alias Model =
  { reviews : Maybe (List Review)
  , newReviewForm : NewReviewForm Msg
  , user : User
  }

type Msg
  = GotReviews (Result Http.Error (List Review))
  | NewReviewBoxTextChanged String
  | NewReviewPostClicked
  | GotNewReview (Result Http.Error Review)
  | NewReviewFormStarsRadioChanged Int
  | NewReviewFormSubjectQueryChanged String

init : () -> (Model, Cmd Msg)
init _ =
  let
    initModel =
        { reviews = Nothing
        , newReviewForm = emptyNewReviewForm
        , user =
          { id = Just 67
          , image = Just "../static/star.png"
          , username = "angusjf"
          }
        }
  in
    (initModel, getReviews)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotReviews result ->
      case (log "got reviews" result) of
        Ok reviews -> ({model | reviews = Just reviews}, Cmd.none)
        Err _      -> (model, Cmd.none)
    NewReviewBoxTextChanged newText ->
        ({ model | newReviewForm = setReviewFormText model.newReviewForm newText }, Cmd.none)
    NewReviewPostClicked ->
      case (log "eee: " (convertToNewReview model.newReviewForm model.user)) of
        Just newReview ->
          let
            newModel = { model | newReviewForm = emptyNewReviewForm }
          in
            (newModel, postReview newReview)
        Nothing -> (model, Cmd.none)
    GotNewReview result ->
      case (log "new review" result) of
        Ok newReview ->
          ({ model | reviews =
            case model.reviews of
              Just rs -> Just <| newReview :: rs
              Nothing -> Just <| newReview :: []
           }
          , Cmd.none)
        Err _        -> (model, Cmd.none)
    NewReviewFormStarsRadioChanged n ->
      ({ model | newReviewForm = setReviewFormStars model.newReviewForm n }, Cmd.none)
    NewReviewFormSubjectQueryChanged newQuery ->
      ({ model | newReviewForm = setReviewFormSubjectQuery model.newReviewForm newQuery}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Document Msg
view model =
  { title = "songscore v99"
  , body = [ Element.layout [] (viewPage model) ]
  }

viewPage : Model -> Element Msg
viewPage model =
  column []
    [ viewNewReviewForm model.newReviewForm
    , case model.reviews of
        Just reviews ->
          column [] <| map viewReview reviews
        Nothing -> Element.text "loading..."
    ]

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- HTTP

getReviews : Cmd Msg
getReviews =
  Http.get
    { url = "http://localhost:8000/review"
    , expect = Http.expectJson GotReviews (D.list reviewDecoder)
    }

postReview : Review -> Cmd Msg
postReview review =
  Http.post
    { url = "http://localhost:8000/review"
    , body = Http.jsonBody (encodeReview review)
    , expect = Http.expectJson GotNewReview reviewDecoder
    }

emptyNewReviewForm : NewReviewForm Msg
emptyNewReviewForm =
  { text = Nothing
  , stars = Nothing
  , subject = Nothing
  , subjectQuery = Nothing
  , onPress = NewReviewPostClicked
  , onChange = NewReviewBoxTextChanged
  , starsRadioChanged = NewReviewFormStarsRadioChanged
  , onSubjectQueryChanged = NewReviewFormSubjectQueryChanged
  }
