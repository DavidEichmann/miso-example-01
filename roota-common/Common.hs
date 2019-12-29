{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Common where

import           Control.DeepSeq
import           Data.Aeson hiding (Success)
import           GHC.Generics

type ServerToClientMsg = (Model, Maybe (RequestId, Response))

data Response = Response
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

data ClientMsg = Inc | Dec
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

data RespStatus = Success | Failure
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

newtype RequestId = RequestId Int
    deriving newtype  (Eq, Ord, Num)
    deriving stock    (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, FromJSONKey, ToJSONKey, NFData)
    
data Model = Model Int
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

