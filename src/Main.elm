module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, el, text, row, column, spacing, padding, link)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Jwt
import Jwt.Http
import Jwt.Decoders
import Maybe exposing (Maybe)
import Json.Decode as D
import Json.Encode as E
import List exposing (map)
import Debug exposing (log)
import Review exposing (..)
import User exposing (..)
import Subject exposing (..)
import NewReviewForm exposing (..)
import Url
import Url.Parser as UrlP exposing ((</>))
import Types exposing (..)

type alias Model =
  { reviews : Maybe (List Review)
  , newReviewForm : NewReviewForm Msg
  , user : Maybe User
  , username : String
  , password : String
  , token : String
  , key : Nav.Key
  , url : Url.Url
  }

type Route
  = UserRoute String
  | ReviewRoute String Int
  | FeedRoute
  | LogInRoute
  | RootRoute

-- port storeCache : Maybe Value -> Cmd msg

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  let
    initModel =
        { reviews = Nothing
        , newReviewForm = emptyNewReviewForm
        , user = Nothing
        , username = ""
        , password = ""
        , token = ""
        , key = key
        , url = url
        }
  in
    (initModel, Cmd.none)

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
      case Maybe.andThen (convertToNewReview model.newReviewForm) model.user of
        Just newReview ->
          let
            newModel = { model | newReviewForm = emptyNewReviewForm }
          in
            (newModel, postReview model.token newReview)
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
    OnUsernameChanged newUsername ->
      ({ model | username = newUsername}, Cmd.none)
    OnPasswordChanged newPassword ->
      ({ model | password = newPassword}, Cmd.none)
    LogInPressed ->
      (model, postLogin model.username model.password)
    GotToken result ->
      case (log "got token" result) of
        Ok token -> ({ model | token = token }, getMe token)
        Err _    -> (model, Cmd.none)
    GotMe result ->
      case (log "got me" result) of
        Ok user -> ({ model | user = Just user }, getReviews model.token)
        Err _   -> (model, Cmd.none)
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
            (model, Nav.pushUrl model.key (Url.toString url))
        Browser.External href ->
            (model, Nav.load href)
    UrlChanged url ->
      case url.path of
        _ -> ({ model | url = url }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Browser.Document Msg
view model =
  let
    rootTitle : String
    rootTitle = "SongScore: "
  in
    case model.user of
      Just user ->
        { title = rootTitle ++ "Feed"
        , body = htmlify <| (viewFeedPage model)
        }
      Nothing ->
        { title = rootTitle ++ "Login"
        , body = htmlify <| (viewLoginPage model)
        }

viewFeedPage : Model -> Element Msg
viewFeedPage model =
  column []
    [ viewNewReviewForm model.newReviewForm
    , case model.reviews of
        Just reviews ->
          column [] <| map viewReview reviews
        Nothing -> text "loading..."
    ]

viewLoginPage : Model -> Element Msg
viewLoginPage model =
  column []
    [ Input.username []
        { onChange = OnUsernameChanged
        , text = model.username
        , placeholder = Just (Input.placeholder [] (text "username"))
        , label = Input.labelAbove [] (text "Username")
        }
    , Input.currentPassword []
        { onChange = OnPasswordChanged
        , text = model.password
        , placeholder = Just (Input.placeholder [] (Element.text "password"))
        , label = Input.labelAbove [] (text "Password")
        , show = False
        }
    , Input.button []
      { onPress = Just LogInPressed
      , label = text "Log In"
      }
    ]

viewErrorPage : Model -> Element Msg
viewErrorPage model =
  column []
    [ el [] <| Element.text "That page doesn't exist :("
    , link []
        { url = "/login"
        , label = text "Log in"
        }
    ]

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- HTTP

apiRoot : String
apiRoot = "https://songscore.herokuapp.com"

getReviews : String -> Cmd Msg
getReviews token =
  Jwt.Http.get
    token
    { url = apiRoot ++ "/api/reviews"
    , expect = Http.expectJson GotReviews (D.list reviewDecoder)
    }

postReview : String -> Review -> Cmd Msg
postReview token review =
  Jwt.Http.post
    token
    { url = apiRoot ++ "/api/reviews"
    , body = Http.jsonBody (encodeReview review)
    , expect = Http.expectJson GotNewReview reviewDecoder
    }

postLogin : String -> String -> Cmd Msg
postLogin username password =
  let
    body =
      Http.jsonBody <|
      E.object <|
        [ ("username", E.string username)
        , ("password", E.string password)
        ]
  in
    Http.post
      { url = apiRoot ++ "/api/auth"
      , body = body
      , expect = Http.expectString GotToken
      }

getMe : String -> Cmd Msg
getMe token =
  Jwt.Http.get
    token
    { url = apiRoot ++ "/api/me"
    , expect = Http.expectJson GotMe userDecoder
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

htmlify elem = [ Element.layout [] elem ]

routeParser : UrlP.Parser (Route -> a) a
routeParser =
  UrlP.oneOf
    [ UrlP.map UserRoute     (UrlP.s "user" </> UrlP.string)
    , UrlP.map ReviewRoute   (UrlP.s "user" </> UrlP.string
                          </> UrlP.s "review" </> UrlP.int)
    , UrlP.map FeedRoute     (UrlP.s "feed")
    , UrlP.map LogInRoute    (UrlP.s "login")
    , UrlP.map RootRoute     (UrlP.top)
    ]
