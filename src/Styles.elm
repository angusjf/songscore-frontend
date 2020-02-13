module Styles exposing (..)

import Element as E exposing (Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Html
import User exposing (User)
import Review exposing (Review)
import Browser

red = E.rgb 1.0 0.4 0.4
white = E.rgb 1.0 1.0 1.0
veryLightAlpha = E.rgba 0 0 0 0.1

roundedSmall = Border.rounded 8

roundedTL =
  Border.roundEach
    { topLeft = 8
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }

paddingMixedSmall = E.paddingXY 8 4
paddingMixedMedium = E.paddingXY 16 8

paddingSmall = E.paddingXY 8 8
paddingMedium = E.paddingXY 16 16
spacingSmall = E.spacing 4
spacingMedium = E.spacing 16

text str = E.el [ Font.size 16 ] <| E.text str
textAlt str = E.el [ Font.size 16, Font.color white, Font.bold ] <| E.text str
boldText str = E.el [ Font.size 16, Font.bold ] <| E.text str

button str action =
  Input.button
    [ Background.color red
    , Font.color white
    , Font.bold
    , paddingSmall
    , roundedSmall
    ]
    { onPress = action
    , label = text str
    }

buttonAlt str action =
  Input.button
    [ Background.color white
    , Font.color red
    , Font.bold
    , paddingSmall
    , roundedSmall
    ]
    { onPress = action
    , label = text str
    }

labelSmall str =
  Input.labelAbove [] (textSmall str)

textSmall str = E.el [ Font.size 16 ] (E.text str)

borderSmall = Border.width 3

squareMedium =
--  [ E.width (E.maximum 200 E.fill)
--  , E.height (E.maximum 200 E.fill)
--  ]
  [ E.width (px 140)
  , E.height (px 140)
  ]

circleSmall =
  [ E.width (px 70)
  , E.height (px 70)
  , Border.rounded 140
  ]

lightShadow =
  Border.shadow
    { offset = (0, 0)
    , size = 0
    , blur = 15
    , color = veryLightAlpha
    }

{-
onEnter : msg -> Element.Attribute msg
onEnter msg =
  let
   decoder = D.string |> D.andThen checkEnter
   checkEnter key = if key == "Enter"
                      then D.succeed msg
                      else D.fail "Not the enter key"
  in
    Element.htmlAttribute <|
      Html.Events.on "keyup" <|
        D.field "key" decoder
        -}

----- now for something different

skeleton : Element msg -> Element msg -> Html.Html msg
skeleton bar body =
  E.layout [ E.width E.fill ] <|
    E.column
      [ E.width (E.maximum 1000 E.fill)
      , E.centerX
      ]
      [ bar
      , E.el [ E.padding 8 ] body
      ]

loading : Maybe a -> (a -> Element msg) -> Element msg
loading maybeA showA =
  case maybeA of
    Just a -> showA a
    Nothing -> text "Loading..."

page : List (Element msg) -> Element msg
page contents =
  E.column [ E.width E.fill, spacingMedium ] contents

userProfile : String -> Maybe String -> Element msg
userProfile username maybeImage =
  E.row
    [ E.width E.fill
    , spacingMedium
    ] <|
    case maybeImage of
      Just image ->
        [ E.image squareMedium
          { src = image
          , description = "profile picture"
          }
        , text username
        ]
      Nothing -> [ text username ]

contentList : List (Element msg) -> Element msg
contentList content = E.column [ E.width E.fill , spacingMedium ] content

--------------------- REVIEWBOX

viewReview : Maybe User -> Review -> String
             -> msg -> msg -> msg -> (String -> msg) -> (String -> msg) -> Element msg
viewReview maybeUser review newComment onDelete
          onLike onDislike onCommentChanged onCommentPost =
  case maybeUser of
    Just user ->
      if user.id == review.user.id then
        reviewBoxOwn
          { comments = List.map (\c -> (c.user.username, c.text)) review.comments
          , onDelete = Just onDelete
          , stars = review.stars
          , words = review.text
          , artist = review.subject.artist
          , art = review.subject.image
          , title = review.subject.title
          , picture = review.user.image
          , username = review.user.username
          , likes = List.map .username review.likes
          , dislikes = List.map .username review.dislikes
          , newComment = newComment
          , onCommentChanged = onCommentChanged
          , onCommentPost = onCommentPost
          }
      else
        reviewBoxOther
          { comments = List.map (\c -> (c.user.username, c.text)) review.comments
          , onDislike = Just onDislike
          , onLike = Just onLike
          , stars = review.stars
          , words = review.text
          , artist = review.subject.artist
          , art = review.subject.image
          , title = review.subject.title
          , picture = review.user.image
          , username = review.user.username
          , likes = List.map .username review.likes
          , dislikes =  List.map .username review.dislikes
          , newComment = newComment
          , onCommentChanged = onCommentChanged
          , onCommentPost = onCommentPost
          }
    Nothing ->
      reviewBoxGuest
        { comments = List.map (\c -> (c.user.username, c.text)) review.comments
        , stars = review.stars
        , words = review.text
        , artist = review.subject.artist
        , art = review.subject.image
        , title = review.subject.title
        , picture = review.user.image
        , username = review.user.username
        , likes = List.map .username review.likes
        , dislikes =  List.map .username review.dislikes
        }

reviewBoxOwn :
  { art : Maybe String
  , artist : Maybe String
  , title : String
  , username : String
  , picture : Maybe String
  , onDelete : Maybe msg
  , stars : Int
  , words : Maybe String
  , comments : List (String, String)
  , likes : List String
  , dislikes : List String
  , onCommentChanged : String -> msg
  , newComment : String
  , onCommentPost : String -> msg
  }
  -> Element msg
reviewBoxOwn
  { art, artist, title, username, picture , onDelete, stars, words,
  comments, likes, dislikes, onCommentChanged, newComment, onCommentPost }
  =
    let
      review =
        reviewBoxGeneric
          { art      = art
          , artist   = artist
          , title    = title
          , username = username
          , picture  = picture
          , stars    = stars
          , words    = words
          , comments = comments
          , likes    = likes
          , dislikes = dislikes
          }
          [ deleteButton onDelete
          ]
      newCommentBox =
        E.row [spacingMedium]
          [ Input.text []
            { label = Input.labelHidden "comment"
            , onChange = onCommentChanged
            , placeholder =
                Just (Input.placeholder [] <| text "leave a comment...")
            , text = newComment
            }
          , button "post" <| Just (onCommentPost newComment)
          ]
    in
      outerBox <|
        if List.isEmpty comments then
          [ review, newCommentBox ]
        else
          [ review
          , newCommentBox
          , commentsBox comments
          ]

reviewBoxOther :
  { art : Maybe String
  , artist : Maybe String
  , title : String
  , username : String
  , picture : Maybe String
  , onLike : Maybe msg
  , onDislike : Maybe msg
  , stars : Int
  , words : Maybe String
  , comments : List (String, String)
  , likes : List String
  , dislikes : List String
  , onCommentChanged : String -> msg
  , newComment : String
  , onCommentPost : String -> msg
  }
  -> Element msg
reviewBoxOther
  { art, artist, title, username, picture, onLike, onDislike, stars, words
  , comments, likes, dislikes, onCommentChanged, newComment, onCommentPost }
  =
    let
      review =
        reviewBoxGeneric
          { art      = art
          , artist   = artist
          , title    = title
          , username = username
          , picture  = picture
          , stars    = stars
          , words    = words
          , comments = comments
          , likes    = likes
          , dislikes = dislikes
          }
          [ likeButton onLike
          , dislikeButton onDislike
          ]
      newCommentBox =
        E.row [spacingMedium]
          [ Input.text []
            { label = Input.labelHidden "comment"
            , onChange = onCommentChanged
            , placeholder =
                Just (Input.placeholder [] <| text "leave a comment...")
            , text = newComment
            }
          , button "post" <| Just (onCommentPost newComment)
          ]
    in
      outerBox <|
        if List.isEmpty comments then
          [ review, newCommentBox ]
        else
          [ review
          , newCommentBox
          , commentsBox comments
          ]

reviewBoxGuest :
  { art : Maybe String
  , artist : Maybe String
  , title : String
  , username : String
  , picture : Maybe String
  , stars : Int
  , words : Maybe String
  , comments : List (String, String)
  , likes : List String
  , dislikes : List String
  }
  -> Element msg
reviewBoxGuest
  { art, artist, title, username, picture
  , stars, words, comments, likes, dislikes }
  =
    let
      review =
        reviewBoxGeneric
          { art      = art
          , artist   = artist
          , title    = title
          , username = username
          , picture  = picture
          , stars    = stars
          , words    = words
          , comments = comments
          , likes    = likes
          , dislikes = dislikes
          }
          []
    in
      outerBox <|
        if List.isEmpty comments
          then [ review ]
          else [ review, commentsBox comments ]

reviewBoxGeneric :
  { art : Maybe String
  , artist : Maybe String
  , title : String
  , username : String
  , picture : Maybe String
  , stars : Int
  , words : Maybe String
  , comments : List (String, String)
  , likes : List String
  , dislikes : List String
  }
  -> List (Element msg)
  -> Element msg
reviewBoxGeneric
  { art, artist, title, username, picture, stars
  , words, comments, likes, dislikes }
  actions
  =
        E.row
          [ E.width E.fill
          , E.height E.fill
          ]
          [ subjectBox { artist = artist, title = title, image = art }
          , E.column
              [ spacingMedium
              , E.alignTop
              , E.width E.fill
              , E.height E.fill
              , bottomLine
              , Border.color veryLightAlpha
              , paddingSmall
              ]
              [ E.column
                  [ E.alignTop
                  , spacingMedium
                  , E.height E.fill
                  ] <|
                  case (List.isEmpty likes, List.isEmpty dislikes) of
                    (True, True) ->
                      [ nStars stars
                      , reviewTextBox words
                      ]
                    (True, False) ->
                      [ nStars stars
                      , reviewTextBox words
                      , dislikesBox dislikes
                      ]
                    (False, True) ->
                      [ nStars stars
                      , reviewTextBox words
                      , likesBox likes
                      ]
                    (False, False) ->
                      [ nStars stars
                      , reviewTextBox words
                      , likesBox likes
                      , dislikesBox dislikes
                      ]
              , E.row
                  [ spacingMedium
                  , E.alignBottom
                  , E.height (E.px 40)
                  ] <|
                  actions ++ [ E.el [E.alignRight] <| userBox username picture ]
              ]
          ]

bottomLine =
  Border.widthEach
    { bottom = 1
    , left = 0
    , right = 0
    , top = 0
    }

joinAllNice : List String -> String
joinAllNice strs =
  case strs of
    x::y::[]   -> x ++ " & " ++ y
    x::y::rest -> x ++ ", " ++ (joinAllNice rest)
    x::[]      -> x
    []         -> ""

likesBox : List String -> Element msg
likesBox usernames =
  text <|
    case usernames of
      _::_ ->
        (joinAllNice usernames) ++ " agreed with this!"
      [] ->
        ""

dislikesBox usernames =
  text <|
    case usernames of
      _::_ ->
        (joinAllNice usernames) ++ " disagreed with this"
      [] ->
        ""

outerBox elems =
  E.column
    [ spacingMedium
    , lightShadow
    --, paddingMedium
    , roundedSmall
    , E.width (E.px 550)
    ]
    elems

subjectBox { artist, title, image } =
  let
    attrs =
      [ E.width (192 |> E.px)
      , E.height (192 |> E.px)
      , paddingMedium
      , E.alignTop
      , roundedTL
      , Background.gradient
          { angle = 3.14
          , steps =
            [ E.rgba 0 0 0 0.1
            , E.rgba 0 0 0 0
            , E.rgba 0 0 0 0
            , E.rgba 0 0 0 0
            ]
          }
      ]
    shadowTextBig str = shadowText str 18
    shadowTextSmall str = shadowText str 15
    shadowText str n =
      E.el
        [ Font.shadow
            { offset = (0, 0)
            , blur = 3
            , color = E.rgba 0 0 0 0.9
            }
        , Font.color white
        , Font.italic
        , Font.bold
        , Font.size n
        ] <|
          E.paragraph [] [ E.text str ]
  in
    E.el
      [ roundedTL
      , Background.image <|
          Maybe.withDefault
          "/assets/images/default-subject.png" <|
          image
      , E.alignTop
      ] <|
      E.el
        attrs <|
        E.column [ spacingSmall ]
          [ shadowTextBig title
          , shadowTextSmall <| Maybe.withDefault "" artist
          ]

reviewTextBox str =
  E.el
    [ Font.size 18
    , Font.italic
    , Font.bold
    ] <|
    E.paragraph [] <|
      [ E.text <|
        Maybe.withDefault
          "(this review has no text)" <|
          Maybe.map (\x -> "“" ++ x ++ "”") str
      ]

commentsBox : List (String, String) -> Element msg
commentsBox comments =
  E.column [] <| --[E.width E.fill] <|
    List.map comment comments

comment : (String, String) -> Element msg
comment (username, commentText) =
  E.row [spacingMedium, paddingSmall]
    [ E.link []
        { url = "/users/" ++ username
        , label = boldText ("@" ++ username ++ ":")
        }
    , text commentText
    ]

likeButton : Maybe msg -> Element msg
likeButton msg = button "<3" msg

dislikeButton : Maybe msg -> Element msg
dislikeButton msg = button ":(" msg

deleteButton : Maybe msg -> Element msg
deleteButton msg = button "delete" msg

userBox : String -> Maybe String -> Element msg
userBox username image =
  E.link [E.height E.shrink]
    { url = "/users/" ++ username
    , label =
        E.row [E.height E.shrink]
          [ E.image [E.height E.shrink, E.width (E.px 30)]
              { src =
                  Maybe.withDefault
                    "/assets/images/default-user.png"
                    image
              , description = "profile picture"
              }
          , text ("@" ++ username)
          ]
    }

nStars : Int -> Element msg
nStars n =
  E.row [spacingSmall] <|
    (List.repeat n redStar) ++ (List.repeat (5 - n) greyStar)

redStar : Element msg
redStar =
  E.image [E.width (E.px 52) , E.height (E.px 52)]
    { src = "/assets/images/red-star.png"
    , description = "red star"
    }

greyStar : Element msg
greyStar =
  E.image [E.width (E.px 52) , E.height (E.px 52)]
    { src = "/assets/images/grey-star.png"
    , description = "grey star"
    }

main =
  Browser.sandbox
    { init = ()
    , view =
      \model ->
        E.layout [E.height E.shrink, E.width E.shrink] <|
          subjectBox
            { artist = Just "Denzel Curry & Kenny Beats"
            , title = "Lay_Up.m4a"
            , image = Just "data:image/jpeg;base64,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"
            }

    , update = \msg model -> model
    }
