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

module Test.Wire.API.Golden.Manual.FeatureConfigEvent where

import Imports
import Wire.API.Event.FeatureConfig
import Wire.API.Team.Feature

testObject_FeatureConfigEvent_1 :: Event
testObject_FeatureConfigEvent_1 = Event Update TeamFeatureFileSharing (EdFeatureWithoutConfigAndLockStatusChanged (TeamFeatureStatusNoConfigAndLockStatus FeatureStatusEnabled Unlocked))

testObject_FeatureConfigEvent_2 :: Event
testObject_FeatureConfigEvent_2 = Event Update TeamFeatureSSO (EdFeatureWithoutConfigChanged (TeamFeatureStatusNoConfig FeatureStatusDisabled))

testObject_FeatureConfigEvent_3 :: Event
testObject_FeatureConfigEvent_3 =
  Event
    Update
    TeamFeatureAppLock
    ( EdFeatureApplockChanged
        ( TeamFeatureStatusWithConfig
            FeatureStatusDisabled
            (TeamFeatureAppLockConfig (EnforceAppLock True) 300)
        )
    )
