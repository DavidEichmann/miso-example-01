{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TupleSections #-}

module UI where

import Miso hiding (App(..))

import Common
import Component

import UI.Login
import UI.LoggedIn

data UI
  = Login     LoginPage
  | LoggedIn  LoggedInPage
  deriving (Eq, Show)

instance Component UI where

  type In UI = Model

  data Msg UI
    = LoginMsg     (Msg LoginPage)
    | LoggedInMsg  (Msg LoggedInPage)
    deriving (Eq, Show)

  updateCompWith comp model responseMay msgMay = case comp of
    Login loginComp -> case msgMay of
      -- No Authentication, just login as the submmitted user.
      Just (LoginMsg loginMsg) -> case loginMsg of
        LoginSuccess -> LoggedIn $ iniLoggedInPage
        _ -> Login $ updateCompWith loginComp model responseMay (Just loginMsg)
      _ -> Login $ updateCompWith loginComp model responseMay Nothing

    LoggedIn liComp -> case msgMay of
      Just (LoggedInMsg liMsg) -> LoggedIn $ updateCompWith liComp model responseMay (Just liMsg)
      _ -> LoggedIn $ updateCompWith liComp model responseMay Nothing

  viewCompWith comp model = body_ []
    -- TODO move to <head>
    [ case comp of
      Login    subComp -> LoginMsg    <$$> viewCompWith subComp model
      LoggedIn subComp -> LoggedInMsg <$$> viewCompWith subComp model
    ]

initCompUI :: UI
initCompUI = Login initLoginPage
