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
{-# LANGUAGE RecordWildCards #-}

module Wire.API.MLS.CommitBundle where

import qualified Data.Swagger as S
import Imports
import Wire.API.MLS.GroupInfoBundle
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

data CommitBundle = CommitBundle
  { cbCommitMsg :: RawMLS (Message 'MLSPlainText),
    cbWelcome :: Maybe (RawMLS Welcome),
    cbGroupInfoBundle :: GroupInfoBundle
  }
  deriving (Eq, Show)

instance ParseMLS CommitBundle where
  parseMLS = CommitBundle <$> parseMLS <*> parseMLSOptional parseMLS <*> parseMLS

instance S.ToSchema CommitBundle where
  declareNamedSchema _ = pure (mlsSwagger "CommitBundle")

instance SerialiseMLS CommitBundle where
  serialiseMLS (CommitBundle commit welcome gi) = do
    serialiseMLS commit
    serialiseMLSOptional serialiseMLS welcome
    serialiseMLS gi
