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
import MusicDatabase as MDB
import Http

type Msg
  = OnTextChanged String
  | OnStarsChanged Int
  | OnSubjectQueryChanged String
  | GotAlbumResults (Result Http.Error (List MDB.Album))
  | GotSongResults (Result Http.Error (List MDB.Song))
  | OnAlbumResultClicked MDB.Album
  | OnSongResultClicked MDB.Song

type alias Form msg =
  { text : Maybe String
  , stars : Maybe Int
  , subject : Maybe Subject
  , subjectQuery : String
  , albumResults : List MDB.Album
  , songResults : List MDB.Song
  , onPress : msg
  , toOuterMsg : Msg -> msg
  }

resultsLimit = 2
minSearchLength = 2

init : (Msg -> msg) -> msg -> Form msg
init toOuterMsg onPress = 
  { text = Nothing
  , stars = Nothing
  , subject = Nothing
  , subjectQuery = ""
  , albumResults = []
  , songResults = []
  , onPress = onPress
  , toOuterMsg = toOuterMsg
  }

update : Form msg -> Msg -> (Form msg, Cmd msg)
update model msg =
  case msg of
    OnTextChanged new ->
      ({ model | text = Just new }, Cmd.none)
    OnStarsChanged n ->
      ({ model | stars = Just n }, Cmd.none)
    OnSubjectQueryChanged new ->
          if String.length model.subjectQuery < minSearchLength
            then ( { model
                     | subjectQuery = new
                     , songResults = []
                     , albumResults = []
                   }
                 , Cmd.none
                 )
            else ( { model | subjectQuery = new }
                 , Cmd.batch
                     [ MDB.searchAlbums model.subjectQuery resultsLimit <|
                         \x -> model.toOuterMsg (GotAlbumResults x)
                     , MDB.searchSongs model.subjectQuery <|
                         \x -> model.toOuterMsg (GotSongResults x)
                     ]
                 )
    GotAlbumResults res ->
      case res of
        Ok results ->
          ({ model | albumResults = List.take resultsLimit results }, Cmd.none)
        Err results -> (model, Cmd.none) 
    GotSongResults res ->
      case Debug.log "^^^" res of
        Ok results ->
          ({ model | songResults = List.take resultsLimit results }, Cmd.none)
        Err results -> (model, Cmd.none) 
    OnAlbumResultClicked album ->
      ({ model
         | subject = Just (albumResultToSubject album)
         , albumResults = []
         , songResults = []
         , subjectQuery = album.name ++ " - " ++ album.artist
       }
      , Cmd.none
      )
    OnSongResultClicked song ->
      ({ model
         | subject = Just (songResultToSubject song)
         , albumResults = []
         , songResults = []
         , subjectQuery = song.name ++ " - " ++ song.artist
       }
      , Cmd.none
      )

songResultToSubject : MDB.Song -> Subject
songResultToSubject song =
  { id = Nothing
  , image = Just song.imageUrl
  , kind = Just Song
  , title = song.name
  , artist = Just song.artist
  }

albumResultToSubject : MDB.Album -> Subject
albumResultToSubject song =
  { id = Nothing
  , image = Just song.imageUrl
  , kind = Just Album
  , title = song.name
  , artist = Just song.artist
  }

view : Form msg -> Element msg
view form =
  E.column [S.roundedSmall, Border.color S.red, S.borderSmall, S.paddingSmall]
    [ E.text "Write a Review!"
    , viewSubjectForm form
    , viewStars form
    , viewMultiline form
    , viewSubmitButton form
    ]

viewSubjectForm : Form msg -> Element msg
viewSubjectForm form =
  E.column [S.paddingSmall]
    [ Input.text []
        { onChange = \s -> form.toOuterMsg (OnSubjectQueryChanged s)
        , text = form.subjectQuery
        , placeholder = Just (Input.placeholder [] (E.text "search for a subject"))
        , label = Input.labelAbove [] (E.text "Subject...")
        }
    , E.row []
        [ E.column [S.paddingSmall] <|
            List.map (viewAlbumResults form.toOuterMsg) form.albumResults
        , E.column [S.paddingSmall] <|
            List.map (viewSongResults form.toOuterMsg) form.songResults
        ]
    ]

viewAlbumResults : (Msg -> msg) -> MDB.Album -> Element msg
viewAlbumResults toOuterMsg album =
  Input.button []
    { onPress = Just <| toOuterMsg (OnAlbumResultClicked album)
    , label =
        E.column []
          [ E.image S.squareMedium
            { src = album.imageUrl
            , description = ""
            }
          , E.text album.name
          ]
    }

viewSongResults : (Msg -> msg) -> MDB.Song -> Element msg
viewSongResults toOuterMsg song =
  Input.button []
    { onPress = Just <| toOuterMsg (OnSongResultClicked song)
    , label =
        E.column []
          [ E.image S.squareMedium
            { src = song.imageUrl
            , description = ""
            }
          , E.text song.name
          ]
    }

viewStars : Form msg -> Element msg
viewStars form = 
  case form.stars of 
    Just n ->
      E.row [] <| 
        List.map2 (\f x -> f (form.toOuterMsg (OnStarsChanged x)))
          ((List.repeat n redStar) ++ (List.repeat (5 - n) greyStar)) [1, 2, 3, 4, 5]
    Nothing ->
      E.row [] <| 
        List.map2 (\f x -> f (form.toOuterMsg (OnStarsChanged x)))
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

viewSubmitButton : Form msg -> Element msg
viewSubmitButton form =
  Input.button []
    { onPress = Just form.onPress
    , label = E.text "Post!"
    }

viewMultiline : Form msg -> Element msg
viewMultiline form =
  Input.multiline
    []
    { onChange = \s -> form.toOuterMsg (OnTextChanged s)
    , text = Maybe.withDefault "" form.text
    , placeholder = Just (Input.placeholder [] (E.text "type your review here!"))
    , label = Input.labelAbove [] (E.text "Review text")
    , spellcheck = True
    }

convertToReview : Form msg -> User -> Maybe Review
convertToReview form user =
  case form.stars of
    Just stars ->
      let
        subject = 
          case form.subject of
            Just s -> s
            Nothing ->
              { id = Nothing
              , image = Nothing
              , kind = Nothing
              , title = form.subjectQuery
              , artist = Nothing
              }
      in
        Just
          { id = Nothing
          , text = form.text
          , stars = stars
          , user = user
          , subject = subject
          }
    Nothing -> Nothing
