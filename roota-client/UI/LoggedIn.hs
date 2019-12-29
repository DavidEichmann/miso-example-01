{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

module UI.LoggedIn where

import Common hiding (Admin)
import Component

import Miso hiding (App(..))
import Miso.String

data LoggedInPage = LoggedInPage
  deriving (Eq, Show)

instance Component LoggedInPage where

  type In LoggedInPage = Model

  data Msg LoggedInPage = LoggedInPage_Undefined
    deriving (Eq, Show)

  updateCompWith
    comp
    model
    responseMay
    msgMay
    = LoggedInPage

  viewCompWith comp model =
    div_ [] [
      div_ [ class_ "level" ] [
        div_ [ class_ "level-left"] [
          button_ [ class_ "button", onClick (mkReq $ Dec) ] [ text "-" ],
          div_ [] [ text (toMisoString $ show model) ],
          button_ [ class_ "button", onClick (mkReq $ Inc) ] [ text "+" ]
        ]
      ]
    ]

iniLoggedInPage :: LoggedInPage
iniLoggedInPage = LoggedInPage