{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import           Control.Exception
import           Data.List ( nub )
import qualified Data.List as List
import qualified Data.Map as M

import Common
import ServerLib

main :: IO ()
main = do
    runServer' "127.0.0.1" 9166
        initialModel
        update
        (\ newModel -> putStrLn $ "Model updated: " ++ show newModel)

initialModel :: Model
initialModel = Model 0

-- TODO permisions
update :: ClientMsg -> Model -> (Model, Response)
update msg (Model x) = case msg of
    Inc -> (Model (x+1), Response)
    Dec -> (Model (x-1), Response)
