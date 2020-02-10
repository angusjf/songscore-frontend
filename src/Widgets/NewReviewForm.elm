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

update : Msg -> Form msg -> (Form msg, Cmd msg)
update msg model =
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
         , subjectQuery = album.artist ++ " - " ++ album.name 
       }
      , Cmd.none
      )
    OnSongResultClicked song ->
      ({ model
         | subject = Just (songResultToSubject song)
         , albumResults = []
         , songResults = []
         , subjectQuery = song.artist ++ " - " ++ song.name
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
    albums =
      E.column [S.paddingSmall, E.width E.fill, E.alignTop] <|
               List.map (viewAlbumResult form.toOuterMsg) form.albumResults
    songs =
      E.column [S.paddingSmall, E.width E.fill, E.alignTop] <|
              List.map (viewSongResult form.toOuterMsg) form.songResults
  in
    E.column [S.paddingSmall] <|
      case (List.isEmpty form.albumResults, List.isEmpty form.songResults) of
        (True, True) -> [searchBox]
        _ -> [searchBox, E.row [] [albums, songs]]

viewAlbumResult : (Msg -> msg) -> MDB.Album -> Element msg
viewAlbumResult toOuterMsg album =
  Input.button [E.width E.fill]
    { onPress = Just <| toOuterMsg (OnAlbumResultClicked album)
    , label =
        E.column [ E.width E.fill ]
          [ E.image S.squareMedium
            { src = album.imageUrl
            , description = ""
            }
          , E.paragraph [ E.width E.fill ] <| [ S.text album.name ]
          ]
    }

viewSongResult : (Msg -> msg) -> MDB.Song -> Element msg
viewSongResult toOuterMsg song =
  Input.button [E.width E.fill]
    { onPress = Just <| toOuterMsg (OnSongResultClicked song)
    , label =
        E.column [E.width E.fill]
          [ E.image S.squareMedium
            { src = song.imageUrl
            , description = ""
            }
          , E.paragraph [ E.width E.fill ] <| [ S.text song.name ]
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
viewMultiline form = E.el [ S.paddingSmall ] <|
  Input.multiline
    []
    { onChange = \s -> form.toOuterMsg (OnTextChanged s)
    , text = Maybe.withDefault "" form.text
    , placeholder = Just (Input.placeholder [] (E.text "type your review here!"))
    , label = Input.labelHidden "Review text"
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
          , comments = []
          , likes = []
          , dislikes = []
          }
    Nothing -> Nothing
