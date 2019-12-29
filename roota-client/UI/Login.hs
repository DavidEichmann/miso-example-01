{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module UI.Login (
    LoginPage, Msg (..),
    initLoginPage) where

import Miso hiding (App(..))
import Miso.String
-- import Miso.Html.Element

import Common
import Component

import qualified Bulma as B

data LoginPage = LoginPage
    deriving (Eq, Show)

instance Component LoginPage where

    type In LoginPage = Model

    data Msg LoginPage = LoginSuccess
        deriving (Eq, Show)

    viewCompWith comp model
        = div_ [ class_ "hero is-fullheight" ]
            [ div_ [ class_ "hero-body" ]
                [ div_ [ class_ "container" ]
                    [ form_
                        [ onSubmit (mkMsg $ LoginSuccess) ]
                        [ div_ [ class_ "field is-pulled-right" ]
                            [ div_ [ class_ "control" ]
                                [ button_
                                    [ class_ "button is-link"
                                    , type_ "submit"
                                    ]
                                    [ text "Login" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]

    updateCompWith _comp _model _respMay _msgMay = LoginPage

initLoginPage :: LoginPage
initLoginPage = LoginPage
