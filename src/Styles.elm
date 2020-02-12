module Styles exposing (..)

import Element as E exposing (Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Html
import User exposing (User)
import Review exposing (Review)

red = E.rgb 1.0 0.4 0.4
white = E.rgb 1.0 1.0 1.0
veryLightAlpha = E.rgba 0 0 0 0.1

roundedSmall = Border.rounded 8

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
      if user == review.user then
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
          , text "where is it?"
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
  , newComment : String
  }
  -> Element msg
reviewBoxOther
  { art, artist, title, username, picture, onLike, onDislike
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
          [ likeButton onLike
          , dislikeButton onDislike
          ]
    in
      outerBox <|
        if List.isEmpty comments
          then [ review ]
          else [ review, commentsBox comments ]

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
          [ spacingMedium, E.width E.shrink ]
          [ subjectBox { artist = artist, title = title, image = art }
          , E.column [spacingMedium, E.alignTop, E.width E.shrink]
              [ E.column [E.alignTop, spacingMedium, E.width E.shrink]
                  [ nStars stars
                  , reviewTextBox words
                  ]
              , E.row
                  [ spacingMedium
                  , E.alignBottom
                  , E.height (E.px 40)
                  ] <|
                  actions ++ [ E.el [E.alignRight] <| userBox username picture ]
              ]
          ]

outerBox elems =
  E.column
    [spacingMedium, lightShadow, paddingMedium, roundedSmall]
    elems

subjectBox { artist, title, image } =
  E.column [spacingMedium, E.alignTop] 
    [ E.image squareMedium
        { src = Maybe.withDefault
            "/assets/images/default-subject.png"
            image
        , description = "subject picture"
        }
    , E.column [] <|
        [ text <| Maybe.withDefault "" artist
        , text title
        ]
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
