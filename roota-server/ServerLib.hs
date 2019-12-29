{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ServerLib where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Proxy
import qualified Data.ByteString.Lazy as BS
import qualified Network.WebSockets as WS

import Common (RequestId (..))

-- TODO need to make the server take a requestID and clientID with each client msg and return responses with a copy of the requestID to that client along with the updated model
--      ie send (Model, Maybe (RequestId, ClientResp)) instead of just Model




type Client = WS.Connection
newtype ClientId = ClientId Int
    deriving newtype (Eq, Ord, Num)
data ServerState model = ServerState model ClientId [(ClientId, Client)]

runServer'
    :: forall model clientMsg response
    .  ( Show model
       , Show clientMsg
       , FromJSON clientMsg
       , ToJSON   clientMsg
       , FromJSON model
       , ToJSON   model
       , NFData   model
       , FromJSON response
       , ToJSON   response
       , NFData   response
       )
    => String
    -> Int
    -> model
    -> (clientMsg -> model -> (model, response))
    -> (model -> IO ())
    -> IO ()
runServer' address port initialModel update saveModel = do
    putStrLn "# Starting Server"
    state <- newMVar (ServerState initialModel 0 [])

    -- Start thread that processes client msgs.
    msgChan <- forkClientMsgHandler state update saveModel

    -- TODO runServer is not production ready... see (http://hackage.haskell.org/package/websockets-0.12.5.3/docs/Network-WebSockets.html#v:runServer)
    WS.runServer address port $ application (Proxy @response) msgChan state
    putStrLn "# Server Stopped"

-- | Fork a thread that just keeps reading JSON encoded `ClientMsg`s (via
-- the returned chanel), parsing them, updating the model in the
-- `ServerState` and broadcasting the new state to all clients
forkClientMsgHandler
    :: forall model clientMsg response
    .  ( Show model
       , Show clientMsg
       , FromJSON clientMsg
       , ToJSON   clientMsg
       , FromJSON model
       , ToJSON   model
       , NFData   model
       , FromJSON response
       , ToJSON   response
       , NFData   response
       )
    => MVar (ServerState model) 
    -> (clientMsg -> model -> (model, response))
    -> (model -> IO ()) 
    -> IO (Chan (ClientId, BS.ByteString))
forkClientMsgHandler state update saveModel = do
    msgChan :: Chan (ClientId, BS.ByteString) <- newChan
    _ <- forkIO $ forever (processMsg msgChan `catch` (\ (e :: SomeException) -> putStrLn $ "Failed to process msg: " ++ show e))
    return msgChan
    where
        processMsg :: Chan (ClientId, BS.ByteString) -> IO ()
        processMsg msgChan = do
            -- Decode.
            -- TODO handle Nothing case.
            Just (requestClient, clientMsg) <- (\ (cl, bs) -> (cl,) <$> decode bs) <$> readChan msgChan

            -- Update Model.
            mayNewStateAndResp <- modifyMVar state $
                \ oldState@(ServerState model nextClientId clients) -> do
                    putStrLn $ "# Update State"
                    putStrLn $ "# ClientMsg: " ++ show clientMsg

                    -- TODO Performance: serilize/write the whole db!
                    errOrNewModelMayResp <- try $ do
                        let newModelAndResp@(newModel,_) = update clientMsg model
                        saveModel newModel
                        return newModelAndResp
                    case errOrNewModelMayResp of
                        Left (e :: SomeException) -> do
                            putStrLn $ "# Failed to update (skipping):\n" ++ show e
                            return (oldState, Nothing)
                        Right (newModel, response) -> do
                            putStrLn $ "# new Model: " ++ show newModel
                            let newState = ServerState newModel nextClientId clients
                            return (newState, Just (newState, response))

            -- Only if the request succeeded, broadcast new state to clients.
            forM_ mayNewStateAndResp $ \ (ServerState newModel _nextClientId clients, response) -> do
                let newModelNoResp   = (newModel, Nothing)   :: (model, Maybe (RequestId, response))
                    newModelWithResp = (newModel, Just (RequestId 0, response)) -- error "TODO reqId"
                forM_ clients $ \ (otherClientId, conn) -> WS.sendTextData conn (encode $ if otherClientId == requestClient then newModelWithResp else newModelNoResp)
                    `catch` (\ (e :: WS.ConnectionException) -> putStrLn $ "TODO remove closed connections... " ++ show e)

application :: forall model response . (FromJSON model, ToJSON model, ToJSON response)
    => Proxy response -> Chan (ClientId, BS.ByteString) -> MVar (ServerState model) -> WS.ServerApp
application _ msgChan state pending = do
    -- Our application starts by accepting the connection. In a more realistic
    -- application, you probably want to check the path and headers provided by the
    -- pending request.
    conn <- WS.acceptRequest pending
    putStrLn "New Connection"
    (modelInit, clientId) <- modifyMVar state $ \ (ServerState model newClientId clients)
        -> return (ServerState model (newClientId + 1) ((newClientId, conn) : clients), (model, newClientId))
    sendMsgToClient (Proxy @response) conn modelInit Nothing

    -- We also fork a pinging thread in the background. This will ensure the connection
    -- stays alive on some browsers.
    WS.forkPingThread conn 30

    -- Forever receive Msgs and broadcast state updates.
    forever $ do
        d <- WS.receiveData conn
        putStrLn "Msg Received."
        writeChan msgChan (clientId, d)

sendMsgToClient
    :: forall model response
    .  (ToJSON model, ToJSON response)
    => Proxy response
    -> WS.Connection
    -> model
    -> Maybe (RequestId, response)
    -> IO ()
sendMsgToClient _ conn m mayReqIdAndResp = WS.sendTextData conn (encode (m, mayReqIdAndResp))
