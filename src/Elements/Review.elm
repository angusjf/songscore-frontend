module Elements.Review exposing (view)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Styles as S
import Browser
import Review exposing (Review, Comment)
import Session
import Api

type alias ReviewElementArgs msg =
  { review : Review
  , session : Session.Data
  , onDelete : Maybe msg
  , onLike : Maybe msg
  , onDislike : Maybe msg
  , onComment : Maybe (String -> msg)
  }

view : ReviewElementArgs msg -> Element msg
view args =
  E.column [S.spacingMedium, S.lightShadow, S.paddingMedium]
    [ E.row [S.spacingMedium]
        [ E.column [S.spacingMedium, E.alignTop] 
            [ E.image S.squareMedium
                { src = Maybe.withDefault
                    "/assets/images/default-subject.png"
                    args.review.subject.image
                , description = "subject picture"
                }
            , E.column [] <|
                [ S.text <| Maybe.withDefault "" args.review.subject.artist
                , S.text args.review.subject.title
                ]
            ]
        , E.column [S.spacingMedium, E.width (E.px 500), E.alignTop] 
            [ E.row [E.width E.fill]
              [ E.link []
                { url = "/users/" ++ args.review.user.username
                , label =
                    E.row []
                      [ E.image S.circleSmall
                          { src = Maybe.withDefault "/assets/images/default-user.png" args.review.user.image
                          , description = "profile picture"
                          }
                      , S.text ("@" ++ args.review.user.username)
                      ]
                }
              , E.row [E.alignRight] <|
                  case args.session.userAndToken of
                    Just uAndT ->
                      if uAndT.user == args.review.user
                        then [ S.button "delete" <| args.onDelete ]
                        else []
                    Nothing -> []
              ]
            , E.row [S.spacingMedium, E.width E.fill]
                [ E.column [S.spacingMedium, E.width E.fill] <|
                    [ viewNStars args.review.stars
                    , E.paragraph [E.width E.fill] <|
                        [ E.text <|
                            Maybe.withDefault
                              "(this args.review has no text)" <|
                              Maybe.map (\x -> "“" ++ x ++ "”") args.review.text
                        ]
                    ]
                , E.row [S.spacingMedium, E.alignRight, E.alignTop]
                    [ S.button "<3" <| args.onLike
                    , S.button ":(" <| args.onDislike
                    ]
                ]
            ]
        ]
    , E.column [E.width E.fill] <|
        List.map viewComment args.review.comments
    ]

viewComment : Comment -> Element msg
viewComment comment =
  E.row [S.spacingMedium, S.paddingSmall]
    [ E.link []
        { url = "/users/" ++ comment.user.username
        , label = S.boldText ("@" ++ comment.user.username ++ ":")
        }
    , S.text comment.text
    ]
 
viewNStars : Int -> Element msg
viewNStars n =
  E.row [S.spacingSmall] <|
    (List.repeat n redStar) ++ (List.repeat (5 - n) greyStar)

redStar : Element msg
redStar =
  E.image [E.width (E.px 72) , E.height (E.px 72)]
    { src = "/assets/images/red-star.png"
    , description = "red star"
    }

greyStar : Element msg
greyStar =
  E.image [E.width (E.px 72) , E.height (E.px 72)]
    { src = "/assets/images/grey-star.png"
    , description = "grey star"
    }
