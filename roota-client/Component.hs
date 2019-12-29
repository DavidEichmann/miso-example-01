{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Component
    ( CompAction (..)
    , mkNoOp, mkReq, mkMsg, mkReqMsg
    , Component (..)
    , updateComp, viewComp, (<$$>)
    , RequestM
    -- , mkReq'
    ) where

import           Common
import           Miso

data CompAction a = CompAction (Maybe ClientMsg) (Maybe a)
    deriving (Eq, Show, Functor)

mkNoOp :: Component a => CompAction (Msg a)
mkNoOp = CompAction Nothing Nothing

mkReq :: Component a => ClientMsg -> CompAction (Msg a)
mkReq cm = CompAction (Just cm) Nothing

mkMsg :: Component a => Msg a -> CompAction (Msg a)
mkMsg msg = CompAction Nothing (Just msg)

mkReqMsg :: Component a => ClientMsg -> Msg a -> CompAction (Msg a)
mkReqMsg cm msg = CompAction (Just cm) (Just msg)

-- A component pattern.
class Component a where
    -- | The input data e.g. the data model from the server and the current user's id.
    type In a

    -- | UI messag type emited from the View and passed to `updateCompWith` to update
    -- the state of the component.
    data Msg a

    -- | Update the state. Must be called from the partent component whenever
    -- the input (In a) changes or when the view fires a new Msg.
    -- If this componen has various sub components, always update all subcomponents.
    updateCompWith :: a -> In a -> Maybe (RequestId, Response) -> Maybe (Msg a) -> a

    -- | View the component.
    viewCompWith   :: a -> In a -> View (CompAction (Msg a))

updateComp :: (Component a, In a ~ ()) => a -> Maybe (RequestId, Response) -> Maybe (Msg a) -> a
updateComp a = updateCompWith a ()

viewComp :: (Component a, In a ~ ()) => a -> View (CompAction (Msg a))
viewComp a = viewCompWith a ()

(<$$>) :: (Component a, Component b)
       => (Msg a -> Msg b)
       -> View (CompAction (Msg a))
       -> View (CompAction (Msg b))
f <$$> v = fmap f <$> v

-- Often components want a response from a request. We can only have (at the moment)
-- 1 request at a time triggered by the view. So we use a monad with a fresh supply of
-- unique request ids. Then a request can be created with mkReq' which also returns a way
-- to receive a response.
-- TODO just use a state monad (state is the next available RequestId).
data RequestM a = RequestM (RequestId -> (RequestId, a))

-- -- Make a request expecting a response
-- mkReq' :: forall comp req
--         . (Component comp, Typeable req)
--        => req -> RequestM (GetResponse req, CompAction (Msg comp))
-- mkReq' req = do
--     rid <- nextRequestId
--     return (\ respMay' -> case respMay' of
--                 Just (Response' ((Response rid2 resp) :: Response req2))
--                     -> case eqT @req2 @req of
--                         Just Refl
--                             | rid2 == rid
--                             -> Just resp
--                         _ -> Nothing
--                 _ -> Nothing
--             , mkReq (toClientMsg req)
--             )
--
-- nextRequestId :: RequestM RequestId
-- nextRequestId = RequestM (\ rid -> (rid + 1, rid))

instance Functor RequestM where
    fmap f (RequestM run) = RequestM (\ rid -> let (rid', a) = (run rid) in (rid', f a))

instance Applicative RequestM where
    pure a = RequestM (, a)
    (RequestM runF) <*> (RequestM runA) = RequestM (\ rid -> let
        (rid' , f) = runF rid
        (rid'', a) = runA rid'
        in (rid'', f a))

instance Monad RequestM where
    (RequestM runA) >>= b = RequestM (\ rid -> let
        (rid' , a) = runA rid
        RequestM runB = b a
        in runB rid')
