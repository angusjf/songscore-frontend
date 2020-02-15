module Subject exposing (..)

import Json.Decode as D
import Json.Encode as E

type alias Subject =
  { id : Maybe Int
  , image : String
  , kind : SubjectKind
  , title : String
  , artist : String
  , spotifyId : String
  }

type SubjectKind = Album | Song

decoder : D.Decoder Subject
decoder =
  D.map6 Subject
    (D.maybe (D.field "id" D.int))
    (D.field "image" D.string)
    (D.field "kind" subjectKindDecoder)
    (D.field "title" D.string)
    (D.field "artist" D.string)
    (D.field "spotifyId" D.string)

subjectKindDecoder : D.Decoder (SubjectKind)
subjectKindDecoder = D.map toSubjectKind D.string

toSubjectKind : String -> SubjectKind
toSubjectKind kind =
  case kind of
    "Album" -> Album
    _      -> Song

encode : Subject -> E.Value
encode subject =
  let
    id = case subject.id of
      Just i -> E.int i
      Nothing -> E.null
    kind = case subject.kind of
      Album -> E.string "Album"
      Song -> E.string "Song"
  in
    E.object
      [ ("id", id)
      , ("image", E.string subject.image)
      , ("kind", kind)
      , ("title", E.string subject.title)
      , ("artist", E.string subject.artist)
      , ("spotifyId", E.string subject.spotifyId)
      ]
