{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TypeSynonymInstances #-}

module Api where

import           Data.Proxy
import           Servant.API
import qualified Elm.Derive
import           Models

type Api =
  "api" :>
    ("item" :> Get '[JSON] [Int] :<|>
     "item" :> Capture "itemId" Int :> Get '[JSON] Item :<|>
     "item" :> ReqBody '[JSON] Item :> Post '[JSON] Int :<|>
     "item" :> Capture "itemId" Int :> Delete '[JSON] ())

api :: Proxy Api
api = Proxy

-- types

--newtype ItemId = ItemId Int
--  deriving (Show, Eq, Ord, Enum, FromHttpApiData, ToHttpApiData)

--data Item
--  = Item {
--    id :: ItemId,
--    text :: String
--  }
--  deriving (Show, Eq)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Item
--Elm.Derive.deriveElmDef Elm.Derive.defaultOptions ''ItemId
