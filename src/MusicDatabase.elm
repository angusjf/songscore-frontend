module MusicDatabase exposing (Album, Song, searchAlbums, searchSongs, albumResultsDecoder, songResultsDecoder)

import Http
import Url
import Url.Builder as Builder
import Json.Decode as D

geniusToken = "6B5cvLsXuzowBsyhYFzdx4D3ZIEfkd-1VvYII2UXrKp01DZ44EPjAcZMQyUd3dSS"

type alias Album =
  { name : String
  , artist : String
  , imageUrl : String
  }

type alias Song =
  { name : String
  , artist : String
  , imageUrl : String
  }

searchAlbums : String -> Int -> (Result Http.Error (List Album) -> msg) -> Cmd msg
searchAlbums query limit msg =
  Http.get
    { url = buildLastfmUrl "album" query limit
    , expect = Http.expectJson msg albumResultsDecoder
    }

searchSongs : String -> (Result Http.Error (List Song) -> msg) -> Cmd msg
searchSongs query msg =
  Http.get
    { url = buildGeniusUrl query
    , expect = Http.expectJson msg songResultsDecoder
    }

buildLastfmUrl : String -> String -> Int -> String
buildLastfmUrl media query limit =
  Builder.crossOrigin
    "http://ws.audioscrobbler.com"
    [ "2.0" ]
    [ Builder.string "method" (media ++ ".search")
    , Builder.string media query
    , Builder.string "api_key" "b392916683d0336a30882ff34ff114f7"
    , Builder.string "format" "json"
    , Builder.int "limit" limit
    ]

buildGeniusUrl : String -> String
buildGeniusUrl query =
  Builder.crossOrigin
    "https://api.genius.com"
    [ "search" ]
    [ Builder.string "q" query
    , Builder.string "access_token" geniusToken
    ]

albumResultsDecoder : D.Decoder (List Album)
albumResultsDecoder =
  D.at [ "results", "albummatches", "album"] <|
    D.list <|
      D.map3 Album
       (D.field "name" D.string)
       (D.field "artist" D.string)
       (D.field "image" (D.index 3 (D.field "#text" D.string)))

songResultsDecoder : D.Decoder (List Song)
songResultsDecoder = 
  D.at [ "response", "hits"] <|
    D.list <|
      D.field "result" <|
        D.map3 Song
         (D.field "title" D.string)
         (D.at ["primary_artist", "name"] D.string)
         (D.field "song_art_image_url" D.string)
