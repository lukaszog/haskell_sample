{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}

module ModelsJson where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Database.Persist
import           Models

instance ToJSON (Entity User) where
  toJSON (Entity uid (c@User{..})) =
    object
    [ "id" .= uid
    , "name" .= userName
    , "client_id" .= userClient_id
    ]

instance FromJSON User where
    parseJSON (Object v) =
        User <$> v .: "name"
             <*> v .: "client_id"
    parseJSON _ = mzero
