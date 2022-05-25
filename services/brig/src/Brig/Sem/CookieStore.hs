-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Brig.Sem.CookieStore where

import Cassandra (Cql)
import Data.Id
import Imports
import Polysemy
import Wire.API.User.Auth

newtype TTL = TTL {ttlSeconds :: Int32}
  deriving (Cql)

data CookieStore m a where
  GetCookies :: UserId -> CookieStore m [Cookie ()]
  InsertCookie :: UserId -> Cookie a -> Maybe TTL -> CookieStore m ()
  DeleteCookies :: UserId -> [Cookie a] -> CookieStore m ()
  DeleteAllCookies :: UserId -> CookieStore m ()

makeSem ''CookieStore

revokeCookies ::
  Member CookieStore r =>
  UserId ->
  [CookieId] ->
  [CookieLabel] ->
  Sem r ()
revokeCookies u [] [] = deleteAllCookies u
revokeCookies u ids labels = do
  cc <- filter matching <$> getCookies u
  deleteCookies u cc
  where
    matching c =
      cookieId c `elem` ids
        || maybe False (`elem` labels) (cookieLabel c)
