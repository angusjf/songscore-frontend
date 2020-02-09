module MusicDatabase exposing (Album, Song, searchAlbums, searchSongs, albumResultsDecoder, songResultsDecoder)

import Http
import Url
import Url.Builder as Builder
import Json.Decode as D

type alias Album =
  { name : String
  , artist : String
  , imageUrlSmall : String
  , imageUrlLarge : String
  }

type alias Song =
  { name : String
  , artist : String
  , imageUrlSmall : String
  , imageUrlLarge : String
  }

searchAlbums : String -> Int -> (Result Http.Error (List Album) -> msg) -> Cmd msg
searchAlbums query limit msg =
  Http.get
    { url = (buildUrl "album" query limit)
    , expect = Http.expectJson msg albumResultsDecoder
    }

searchSongs : String -> Int -> (Result Http.Error (List Song) -> msg) -> Cmd msg
searchSongs query limit msg =
  Http.get
    { url = (buildUrl "track" query limit)
    , expect = Http.expectJson msg songResultsDecoder
    }

buildUrl : String -> String -> Int -> String
buildUrl media query limit =
  Builder.crossOrigin
    "http://ws.audioscrobbler.com"
    [ "2.0" ]
    [ Builder.string "method" (media ++ ".search")
    , Builder.string media query
    , Builder.string "api_key" "b392916683d0336a30882ff34ff114f7"
    , Builder.string "format" "json"
    , Builder.int "limit" limit
    ]

albumResultsDecoder : D.Decoder (List Album)
albumResultsDecoder =
  D.at [ "results", "albummatches", "album"] <|
    D.list <|
      D.map4 Album
       (D.field "name" D.string)
       (D.field "artist" D.string)
       (D.field "image" (D.index 2 (D.field "#text" D.string)))
       (D.field "image" (D.index 3 (D.field "#text" D.string)))

songResultsDecoder : D.Decoder (List Song)
songResultsDecoder = 
  D.at [ "results", "trackmatches", "track"] <|
    D.list <|
      D.map4 Song
       (D.field "name" D.string)
       (D.field "artist" D.string)
       (D.field "image" (D.index 2 (D.field "#text" D.string)))
       (D.field "image" (D.index 3 (D.field "#text" D.string)))
