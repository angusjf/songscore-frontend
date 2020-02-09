module Subject exposing (..)

import Json.Decode as D
import Json.Encode as E

type alias Subject =
  { id : Maybe Int
  , image : Maybe String
  , kind : Maybe SubjectKind
  , title : String
  , artist : Maybe String
  }

type SubjectKind = Album | Song

decoder : D.Decoder Subject
decoder =
  D.map5 Subject
    (D.maybe (D.field "id" D.int))
    (D.maybe (D.field "image" D.string))
    (D.field "kind" subjectKindDecoder)
    (D.field "title" D.string)
    (D.maybe (D.field "artist" D.string))

subjectKindDecoder : D.Decoder (Maybe SubjectKind)
subjectKindDecoder = D.map toSubjectKind D.string

toSubjectKind : String -> Maybe SubjectKind
toSubjectKind kind =
  case kind of
    "Album" -> Just Album
    "Song" -> Just Song
    _ -> Nothing

encode : Subject -> E.Value
encode subject =
  let
    id = case subject.id of
      Just i -> E.int i
      Nothing -> E.null
    image = case subject.image of
      Just img -> E.string img
      Nothing -> E.null
    kind = case subject.kind of
      Just Album -> E.string "Album"
      Just Song -> E.string "Song"
      Nothing -> E.null
    artist = case subject.artist of
      Just a -> E.string a
      Nothing -> E.null
  in
    E.object
      [ ("id", id)
      , ("image", image)
      , ("kind", kind)
      , ("title", E.string subject.title)
      , ("artist", artist)
      ]
