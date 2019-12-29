{-# LANGUAGE OverloadedStrings #-}

-- HTML with Bulma styling
module Bulma where

import Data.Foldable (toList)

import Miso.String
import Miso.Html
import Miso.Html.Element as Miso

divRight_ :: [View action] -> View action
divRight_ = div_ [ class_ "is-pulled-right" ]

type ColorMod = MisoString
isPrimary, isLink, isInfo, isSuccess, isWarning, isDanger, isDefault :: ColorMod
isPrimary   = "is-primary"
isLink      = "is-link"
isInfo      = "is-info"
isSuccess   = "is-success"
isWarning   = "is-warning"
isDanger    = "is-danger"
isDefault   = ""

container_ :: [View action] -> View action
container_ = div_ [ class_ "container" ]

button'_
    :: MisoString
    -> JSString 
    -> [Attribute action] 
    -> View action
button'_ txt colorMod atrb = div_ [ class_ "control" ]
    [ Miso.button_
        (class_ ("button " `append` colorMod) : atrb)
        [ text txt ]
    ]

buttonWithFieldClass :: MisoString -> MisoString -> ColorMod -> [Attribute action] -> View action
buttonWithFieldClass fieldClass txt colorMod atrb = div_ [ class_ ("field " `append` fieldClass) ] [ button'_ txt colorMod atrb ]
button_, buttonRight_
        :: MisoString           -- ^ text
        -> ColorMod             -- ^ color mod
        -> [Attribute action]   -- ^ attribute
        -> View action
button_ = buttonWithFieldClass ""
buttonRight_ = buttonWithFieldClass "is-pulled-right"

input_ :: Maybe MisoString
       -> Maybe MisoString
       -> Maybe MisoString
       -> [Attribute action]
       -> View action
input_ labelMay placeHolderMay helpMay atrb = div_ [ class_ "field" ] (input'_ labelMay placeHolderMay helpMay atrb)

input'_ :: Maybe MisoString
        -> Maybe MisoString
        -> Maybe MisoString
        -> [Attribute action]
        -> [View action]
input'_ labelMay placeHolderMay helpMay atrb =
        [ div_ [ class_ "label" ] [ text label ]    | label <- toList labelMay ] ++
        [ div_ [ class_ "control" ]
            [ Miso.input_ ([ class_ "input", type_ "text" ]
                            ++ [ placeholder_ ph | ph <- toList placeHolderMay ]
                            ++ atrb) ]

        ] ++
        [ div_ [ class_ "help"  ] [ text help  ]    | help <- toList helpMay ]