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
import List.Extra exposing (unique)

--main =
--  Browser.sandbox
--    { init = ()
--    , view =
--      \model ->
--        E.layout [E.height E.shrink, E.width E.shrink] <|
--          subjectBox
--            { artist = "Denzel Curry & Kenny Beats"
--            , title = "Lay_Up.m4a"
--            , image = "data:image/jpeg;base64,/9j/2wCEAAgGBgcGBQgHBwcJ"
--            }
--
--    , update = \msg model -> model
--    }

red = E.rgb 1.0 0.4 0.4
white = E.rgb 1.0 1.0 1.0
veryLightAlpha = E.rgba 0 0 0 0.1
background = E.rgb 1 1 1
spotifyGreen = E.rgb255 18 208 88

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

squareTiny =
--  [ E.width (E.maximum 200 E.fill)
--  , E.height (E.maximum 200 E.fill)
--  ]
  [ E.width (px 64)
  , E.height (px 64)
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
  E.layout [ E.width E.fill, Background.color background ] <|
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

topLine =
  Border.widthEach
    { bottom = 0
    , left = 0
    , right = 0
    , top = 1
    }

bottomLine =
  Border.widthEach
    { bottom = 1
    , left = 0
    , right = 0
    , top = 0
    }

joinAllNice : List String -> String
joinAllNice joinme =
  let
    helper strs =
      case strs of
        x::y::[]   -> x ++ " & " ++ y
        x::y::rest -> x ++ ", " ++ (helper rest)
        x::[]      -> x
        []         -> ""
   in
     helper (unique joinme)

likesBox : List String -> Element msg
likesBox usernames =
  E.paragraph []
    [ text <|
        case usernames of
          _::_ ->
            (joinAllNice usernames) ++ " agreed with this!"
          [] ->
            ""
    ]

dislikesBox usernames =
  text <|
    case usernames of
      _::_ ->
        (joinAllNice usernames) ++ " disagreed with this"
      [] ->
        ""

outerBox elems =
  E.column
    [ lightShadow
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
      , Background.image image
      , E.alignTop
      ] <|
      E.el
        attrs <|
        E.column [ spacingSmall ]
          [ shadowTextBig title
          , shadowTextSmall artist
          ]

reviewTextBox str =
  E.el
    [ Font.size 16
    , Font.italic
    ] <|
    E.paragraph [] <|
      [ E.text <|
        Maybe.withDefault
          "" <|
          Maybe.map (\x -> "“" ++ x ++ "”") str
      ]

commentsBox : List (User, String) -> Element msg
commentsBox comments =
  E.column
    [ Border.color veryLightAlpha
    , topLine
    , E.width E.fill
    , paddingSmall
    ] <|
    List.map comment comments

comment : (User, String) -> Element msg
comment (user, commentText) =
  E.row [spacingMedium, paddingSmall]
    [ userBox user.username user.image
    , E.paragraph [E.width E.fill] [ text commentText ]
    ]

likeButton : Maybe msg -> Element msg
likeButton msg = button "<3" msg

dislikeButton : Maybe msg -> Element msg
dislikeButton msg = button ":(" msg

deleteButton : Maybe msg -> Element msg
deleteButton msg = button "delete" msg

userBox : String -> Maybe String -> Element msg
userBox username image =
  E.link
    [ E.height E.shrink
    , Background.color veryLightAlpha
    , E.paddingEach { top = 0, left = 0, right = 6, bottom = 0 }
    , roundedSmall
    ]
    { url = "/users/" ++ username
    , label =
        E.row [E.height E.shrink, spacingSmall ]
          [ E.image
              [E.height E.shrink, E.width (E.px 30)]
              { src =
                  Maybe.withDefault
                    "/assets/images/default-user.png"
                    image
              , description = "profile picture"
              }
          , boldText ("@" ++ username)
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

spotifyLink spotifyId =
  E.link []
    { url = "https://open.spotify.com/track/" ++ spotifyId
    , label =
        E.el
          [ Background.color spotifyGreen
          , Font.color white
          , Font.bold
          , paddingSmall
          , roundedSmall
          ] <|
          textAlt "spotify!"
    }

-----------------------------------------------------
-- **---****-----------------------------------------
--*----*-****----------------------------------------
--*----*---------------------------------------------
--*-----***------------------------------------------
-----------------------------------------------------
-----------------------------------------------------

type Kind = Mine | Yours | Guest
viewReview : Maybe User -> Review -> String
             -> msg -> msg -> msg -> (String -> msg) -> (String -> msg) -> Element msg
viewReview maybeUser review newComment onDelete
          onLike onDislike onCommentChanged onCommentPost =
  let
    comments = List.map (\c -> (c.user, c.text)) review.comments
    newCommentBox =
      E.row
        [ paddingSmall
        , spacingMedium
        , E.width E.fill
        ]
        [ Input.text
          [ E.width E.fill
          , E.height (E.px 38)
          ]
          { label = Input.labelHidden "comment"
          , onChange = onCommentChanged
          , placeholder =
              Just <| Input.placeholder [Font.size 14] <|
                  E.text "leave a comment..."
          , text = newComment
          }
        , button "post" <| Just (onCommentPost newComment)
        ]
    spotify = spotifyLink review.subject.spotifyId
    likes = List.map (\u -> u.username) review.likes
    dislikes = List.map (\u -> u.username) review.dislikes
    nameAndText =
      E.column
        [ E.alignTop
        , spacingMedium
        ]
        [ userBox review.user.username review.user.image
        , reviewTextBox review.text
        , text <| Maybe.withDefault "" <| Maybe.map longAgo review.createdAt
        ]
    rvw
      = E.row
          [ E.width E.fill
          , E.height E.fill
          , bottomLine
          , Border.color veryLightAlpha
          ]
          [ subjectBox
              { artist = review.subject.artist
              , title = review.subject.title
              , image = review.subject.image
              }
          , E.column
              [ spacingMedium
              , E.alignTop
              , E.width E.fill
              , E.height E.fill
              , paddingMedium
              ]
              [ E.column
                  [ E.alignTop
                  , spacingMedium
                  , E.height E.fill
                  ] <|
                  case (List.isEmpty review.likes, List.isEmpty review.dislikes) of
                    (True, True) ->
                      [ nStars review.stars
                      , nameAndText
                      ]
                    (True, False) ->
                      [ nStars review.stars
                      , nameAndText
                      , dislikesBox dislikes
                      ]
                    (False, True) ->
                      [ nStars review.stars
                      , nameAndText
                      , likesBox likes
                      ]
                    (False, False) ->
                      [ nStars review.stars
                      , nameAndText
                      , likesBox likes
                      , dislikesBox dislikes
                      ]
              , E.row
                  [ spacingMedium
                  , E.alignBottom
                  , E.height (E.px 40)
                  , E.width E.fill
                  ] <|
                  actions ++ [ spotify ]
              ]
          ]
    actions = 
      case kind of
        Mine ->
          [ deleteButton <| Just onDelete ]
        Yours ->
          [ likeButton <| Just onLike
          , dislikeButton <| Just onDislike
          ]
        Guest ->
          []
    kind = 
      case maybeUser of
        Just user ->
          if user.id == review.user.id then
            -- MY POST
            Mine
          else
            -- YOUR POST
            Yours
        Nothing ->
          -- GUEST
          Guest
  in
    outerBox <|
      if List.isEmpty comments then
        [ rvw
        , newCommentBox
        ]
      else
        [ rvw
        , newCommentBox
        , commentsBox comments
        ]

longAgo : Int -> String
longAgo millis =
    (String.fromFloat ((toFloat millis) / 60 / 60 / 1000)) ++ " hours ago"

-------------------------


