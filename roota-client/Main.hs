{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Miso
import System.IO (hFlush, stdout)

import Language.Javascript.JSaddle.Warp as JSaddle

import Common
import Component

import qualified UI as UI

data ModelAndUi = ModelAndUi (Maybe Model) UI.UI
    deriving (Eq, Show)

-- | Sum type for application events
data Action
    = ModelUpdate Model (Maybe (RequestId, Response))
    | SubCompAction (CompAction (Msg UI.UI))
    | Print String
    | NoOp
    deriving (Show, Eq)

runApp :: JSM () -> IO ()
runApp f =
    JSaddle.run 8080 (f >> syncPoint)

-- | Entry point for a miso application
main :: IO ()
main = runApp $ do
    let wsSub = websocketSub
            (URL "ws://localhost:9166")
            (Protocols [])
            (\case
                WebSocketMessage ((newModel, respIdRespMay) :: ServerToClientMsg) -- Do we support responses yet?
                    -> ModelUpdate newModel respIdRespMay
                x -> Print (show x)
            )

    startApp App
      { initialAction = NoOp          -- initial action to be executed on application load
      , model  = initModelAndUi       -- initial model and ui
      , update = updateModelAndUi     -- update function
      , view   = viewModelAndUi       -- view function
      , events = defaultEvents        -- default delegated events
      , subs   = [wsSub]              -- Subscribe to websocket server
      , mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
      }

initModelAndUi :: ModelAndUi
initModelAndUi = ModelAndUi Nothing UI.initCompUI

-- | Updates model, optionally introduces side effects
updateModelAndUi :: Action -> ModelAndUi -> Effect Action ModelAndUi
updateModelAndUi NoOp cmodel = noEff cmodel
updateModelAndUi (Print x) cmodel = cmodel <# liftIO (putStrLn x >> hFlush stdout >> return NoOp)
updateModelAndUi action (ModelAndUi mMay comp) = (ModelAndUi mMay' comp') <# do
    case uiMsgMay of
        Nothing -> return ()
        Just uiMsg -> liftIO $ do
            putStrLn $ "| UI Msg: " ++ show uiMsg
            hFlush stdout
    case cMsgMay of
        Nothing -> return ()
        Just cMsg -> do
            send cMsg
            liftIO $ do
                putStrLn $ "| Send Request: " ++ show cMsg
                hFlush stdout
    return NoOp
    where
        comp' = fromMaybe comp (updateCompWith <$> pure comp <*> mMay <*> pure respIdRespMay <*> pure uiMsgMay)
        (mMay', respIdRespMay) = case action of
            ModelUpdate m' respIdRespMay' -> (Just m', respIdRespMay')
            _ -> (mMay, Nothing)
        uiMsgMay = case action of
            SubCompAction (CompAction _ (Just uiMsg)) -> Just uiMsg
            _ -> Nothing

        cMsgMay = case action of
            SubCompAction (CompAction (Just cMsg) _) -> Just cMsg
            _ -> Nothing

viewModelAndUi :: ModelAndUi -> View Action
viewModelAndUi (ModelAndUi Nothing _) = text "loading..."
viewModelAndUi (ModelAndUi (Just model') state) = SubCompAction <$> viewCompWith state model'