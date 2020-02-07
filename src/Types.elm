module Types exposing (..)

import Browser.Navigation as Nav
import Url
import Http
import Review exposing (Review)
import User exposing (User)
import Browser

type Msg
  = GotReviews (Result Http.Error (List Review))
  | NewReviewBoxTextChanged String
  | NewReviewPostClicked
  | GotNewReview (Result Http.Error Review)
  | NewReviewFormStarsRadioChanged Int
  | NewReviewFormSubjectQueryChanged String
  | OnUsernameChanged String
  | OnPasswordChanged String
  | LogInPressed
  | GotToken (Result Http.Error String)
  | GotMe (Result Http.Error User)
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | SignUpPressed
  | SignedUp (Result Http.Error User)
