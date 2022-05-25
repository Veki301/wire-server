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

module Brig.Sem.CookieStore.Cassandra (cookieStoreToCassandra) where

import Brig.Sem.CookieStore
import Cassandra
import Data.Id
import Data.Time.Clock
import Imports
import Polysemy
import Wire.API.User.Auth

cookieStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (CookieStore ': r) a ->
  Sem r a
cookieStoreToCassandra =
  interpret $
    embed @m . \case
      GetCookies uid -> listCookies uid
      InsertCookie uid c ttl -> insertCookieQuery uid c ttl
      DeleteCookies id cos -> _wc
      DeleteAllCookies id -> _wd

listCookies :: MonadClient m => UserId -> m [Cookie ()]
listCookies u =
  map toCookie <$> retry x1 (query cql (params LocalQuorum (Identity u)))
  where
    cql :: PrepQuery R (Identity UserId) (CookieId, UTCTime, UTCTime, CookieType, Maybe CookieLabel, Maybe CookieId)
    cql =
      "SELECT id, created, expires, type, label, succ_id \
      \FROM user_cookies \
      \WHERE user = ? \
      \ORDER BY expires ASC"
    toCookie :: (CookieId, UTCTime, UTCTime, CookieType, Maybe CookieLabel, Maybe CookieId) -> Cookie ()
    toCookie (i, ct, et, t, l, sc) =
      Cookie
        { cookieId = i,
          cookieType = t,
          cookieCreated = ct,
          cookieExpires = et,
          cookieLabel = l,
          cookieSucc = sc,
          cookieValue = ()
        }

insertCookieQuery :: MonadClient m => UserId -> Cookie a -> Maybe TTL -> m ()
insertCookieQuery u ck ttl =
  let i = cookieId ck
      x = cookieExpires ck
      c = cookieCreated ck
      t = cookieType ck
      l = cookieLabel ck
      s = cookieSucc ck
      o = fromMaybe (TTL (round (diffUTCTime x c))) ttl
   in retry x5 $ write cql (params LocalQuorum (u, x, i, t, c, l, s, o))
  where
    cql :: PrepQuery W (UserId, UTCTime, CookieId, CookieType, UTCTime, Maybe CookieLabel, Maybe CookieId, TTL) ()
    cql =
      "INSERT INTO user_cookies (user, expires, id, type, created, label, succ_id) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) USING TTL ?"
