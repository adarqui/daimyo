{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Daimyo.Servant.API.Ping (
  getPing
) where

import           Control.Monad.IO.Class
import           Daimyo.Servant.Shared

-- | getPing
getPing :: MonadIO m => Store -> m String
getPing _ = return "pong"
