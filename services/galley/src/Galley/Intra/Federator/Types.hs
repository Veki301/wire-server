{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Intra.Federator.Types
  ( FederatedRPC,
    FederationM,
    runFederationM,
  )
where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Galley.Env
import Galley.Options
import Imports
import Wire.API.Federation.Client
import Wire.API.Federation.GRPC.Types

type FederatedRPC (c :: Component) =
  FederatorClient c (ExceptT FederationClientFailure FederationM)

newtype FederationM a = FederationM
  {unFederationM :: ReaderT Env IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadUnliftIO,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

runFederationM :: Env -> FederationM a -> IO a
runFederationM env = flip runReaderT env . unFederationM

instance HasFederatorConfig FederationM where
  federatorEndpoint = view federator
  federationDomain = view (options . optSettings . setFederationDomain)