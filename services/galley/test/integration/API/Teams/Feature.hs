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

module API.Teams.Feature (tests) where

import API.Util (HasGalley, getFeatureStatusMulti, withSettingsOverrides)
import qualified API.Util as Util
import qualified API.Util.TeamFeature as Util
import Bilge
import Bilge.Assert
import Control.Lens (over, to, view)
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain (..))
import Data.Id
import Data.List1 (list1)
import qualified Data.List1 as List1
import Data.Schema (ToSchema)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE
import Data.Timeout (TimeoutUnit (Second), (#))
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import Network.Wai.Utilities (label)
import Test.Hspec (expectationFailure)
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))
import TestHelpers (test)
import TestSetup
import Wire.API.Event.FeatureConfig (EventData (..))
import qualified Wire.API.Event.FeatureConfig as FeatureConfig
import Wire.API.Internal.Notification (Notification)
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Team.Feature (FeatureTag (..), FeatureStatus (..))
import qualified Wire.API.Team.Feature as Public

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Feature Config API and Team Features API"
    [ test s "SSO" testSSO,
      test s "LegalHold" testLegalHold,
      test s "SearchVisibility" testSearchVisibility,
      test s "DigitalSignatures" $ testSimpleFlag @'Public.TeamFeatureDigitalSignatures Public.FeatureStatusDisabled,
      test s "ValidateSAMLEmails" $ testSimpleFlag @'Public.TeamFeatureValidateSAMLEmails Public.FeatureStatusEnabled,
      test s "FileSharing with lock status" $ testSimpleFlagWithLockStatus @'Public.TeamFeatureFileSharing Public.FeatureStatusEnabled Public.Unlocked,
      test s "Classified Domains (enabled)" testClassifiedDomainsEnabled,
      test s "Classified Domains (disabled)" testClassifiedDomainsDisabled,
      test s "All features" testAllFeatures,
      test s "Feature Configs / Team Features Consistency" testFeatureConfigConsistency,
      test s "ConferenceCalling" $ testSimpleFlag @'Public.TeamFeatureConferenceCalling Public.FeatureStatusEnabled,
      test s "SelfDeletingMessages" testSelfDeletingMessages,
      test s "ConversationGuestLinks - public API" testGuestLinksPublic,
      test s "ConversationGuestLinks - internal API" testGuestLinksInternal,
      test s "ConversationGuestLinks - lock status" $ testSimpleFlagWithLockStatus @'Public.TeamFeatureGuestLinks Public.FeatureStatusEnabled Public.Unlocked,
      test s "SndFactorPasswordChallenge - lock status" $ testSimpleFlagWithLockStatus @'Public.TeamFeatureSndFactorPasswordChallenge Public.FeatureStatusDisabled Public.Locked,
      test s "SearchVisibilityInbound - internal API" testSearchVisibilityInbound,
      test s "SearchVisibilityInbound - internal multi team API" testFeatureNoConfigMultiSearchVisibilityInbound
    ]

testSSO :: TestM ()
testSSO = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getSSO :: HasCallStack => Public.FeatureStatus -> TestM ()
      getSSO = assertFlagNoConfig @'Public.TeamFeatureSSO $ Util.getTeamFeatureFlag Public.TeamFeatureSSO member tid
      getSSOFeatureConfig :: HasCallStack => Public.FeatureStatus -> TestM ()
      getSSOFeatureConfig = assertFlagNoConfig @'Public.TeamFeatureSSO $ Util.getFeatureConfig Public.TeamFeatureSSO member
      getSSOInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      getSSOInternal = assertFlagNoConfig @'Public.TeamFeatureSSO $ Util.getTeamFeatureFlagInternal Public.TeamFeatureSSO tid
      setSSOInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      setSSOInternal = void . Util.putTeamFeatureFlagInternal @'Public.TeamFeatureSSO expect2xx tid . Public.TeamFeatureStatusNoConfig

  assertFlagForbidden $ Util.getTeamFeatureFlag Public.TeamFeatureSSO nonMember tid

  featureSSO <- view (tsGConf . optSettings . setFeatureFlags . flagSSO)
  case featureSSO of
    FeatureSSODisabledByDefault -> do
      -- Test default
      getSSO Public.FeatureStatusDisabled
      getSSOInternal Public.FeatureStatusDisabled
      getSSOFeatureConfig Public.FeatureStatusDisabled

      -- Test override
      setSSOInternal Public.FeatureStatusEnabled
      getSSO Public.FeatureStatusEnabled
      getSSOInternal Public.FeatureStatusEnabled
      getSSOFeatureConfig Public.FeatureStatusEnabled
    FeatureSSOEnabledByDefault -> do
      -- since we don't allow to disable (see 'disableSsoNotImplemented'), we can't test
      -- much here.  (disable failure is covered in "enable/disable SSO" above.)
      getSSO Public.FeatureStatusEnabled
      getSSOInternal Public.FeatureStatusEnabled
      getSSOFeatureConfig Public.FeatureStatusEnabled

testLegalHold :: TestM ()
testLegalHold = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getLegalHold :: HasCallStack => Public.FeatureStatus -> TestM ()
      getLegalHold = assertFlagNoConfig @'Public.TeamFeatureLegalHold $ Util.getTeamFeatureFlag Public.TeamFeatureLegalHold member tid
      getLegalHoldInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      getLegalHoldInternal = assertFlagNoConfig @'Public.TeamFeatureLegalHold $ Util.getTeamFeatureFlagInternal Public.TeamFeatureLegalHold tid
      getLegalHoldFeatureConfig = assertFlagNoConfig @'Public.TeamFeatureLegalHold $ Util.getFeatureConfig Public.TeamFeatureLegalHold member

      setLegalHoldInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      setLegalHoldInternal = void . Util.putTeamFeatureFlagInternal @'Public.TeamFeatureLegalHold expect2xx tid . Public.TeamFeatureStatusNoConfig
  getLegalHold Public.FeatureStatusDisabled
  getLegalHoldInternal Public.FeatureStatusDisabled

  assertFlagForbidden $ Util.getTeamFeatureFlag Public.TeamFeatureLegalHold nonMember tid

  -- FUTUREWORK: run two galleys, like below for custom search visibility.
  featureLegalHold <- view (tsGConf . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledByDefault -> do
      -- Test default
      getLegalHold Public.FeatureStatusDisabled
      getLegalHoldInternal Public.FeatureStatusDisabled
      getLegalHoldFeatureConfig Public.FeatureStatusDisabled

      -- Test override
      setLegalHoldInternal Public.FeatureStatusEnabled
      getLegalHold Public.FeatureStatusEnabled
      getLegalHoldInternal Public.FeatureStatusEnabled
      getLegalHoldFeatureConfig Public.FeatureStatusEnabled

    -- turned off for instance
    FeatureLegalHoldDisabledPermanently -> do
      Util.putLegalHoldEnabledInternal' expect4xx tid Public.FeatureStatusEnabled

    -- turned off but for whitelisted teams with implicit consent
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
      Util.putLegalHoldEnabledInternal' expect4xx tid Public.FeatureStatusEnabled

testSearchVisibility :: TestM ()
testSearchVisibility = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getTeamSearchVisibility :: TeamId -> Public.FeatureStatus -> TestM ()
      getTeamSearchVisibility teamid expected = do
        g <- view tsGalley
        Util.getTeamSearchVisibilityAvailable g owner teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfig expected))

  let getTeamSearchVisibilityInternal :: TeamId -> Public.FeatureStatus -> TestM ()
      getTeamSearchVisibilityInternal teamid expected = do
        g <- view tsGalley
        Util.getTeamSearchVisibilityAvailableInternal g teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfig expected))

  let getTeamSearchVisibilityFeatureConfig :: UserId -> Public.FeatureStatus -> TestM ()
      getTeamSearchVisibilityFeatureConfig uid expected = do
        g <- view tsGalley
        Util.getFeatureConfigWithGalley Public.TeamFeatureSearchVisibility g uid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfig expected))

  let setTeamSearchVisibilityInternal :: TeamId -> Public.FeatureStatus -> TestM ()
      setTeamSearchVisibilityInternal teamid val = do
        g <- view tsGalley
        Util.putTeamSearchVisibilityAvailableInternal g teamid val

  assertFlagForbidden $ Util.getTeamFeatureFlag Public.TeamFeatureSearchVisibility nonMember tid

  tid2 <- Util.createNonBindingTeam "foo" owner []
  team2member <- Util.randomUser
  Util.connectUsers owner (list1 team2member [])
  Util.addTeamMember owner tid2 team2member (rolePermissions RoleMember) Nothing

  Util.withCustomSearchFeature FeatureTeamSearchVisibilityDisabledByDefault $ do
    getTeamSearchVisibility tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibilityInternal tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibilityFeatureConfig team2member Public.FeatureStatusDisabled

    setTeamSearchVisibilityInternal tid2 Public.FeatureStatusEnabled
    getTeamSearchVisibility tid2 Public.FeatureStatusEnabled
    getTeamSearchVisibilityInternal tid2 Public.FeatureStatusEnabled
    getTeamSearchVisibilityFeatureConfig team2member Public.FeatureStatusEnabled

    setTeamSearchVisibilityInternal tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibility tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibilityInternal tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibilityFeatureConfig team2member Public.FeatureStatusDisabled

  tid3 <- Util.createNonBindingTeam "foo" owner []
  team3member <- Util.randomUser
  Util.connectUsers owner (list1 team3member [])
  Util.addTeamMember owner tid3 team3member (rolePermissions RoleMember) Nothing

  Util.withCustomSearchFeature FeatureTeamSearchVisibilityEnabledByDefault $ do
    getTeamSearchVisibility tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibilityInternal tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibilityFeatureConfig team3member Public.FeatureStatusEnabled

    setTeamSearchVisibilityInternal tid3 Public.FeatureStatusDisabled
    getTeamSearchVisibility tid3 Public.FeatureStatusDisabled
    getTeamSearchVisibilityInternal tid3 Public.FeatureStatusDisabled
    getTeamSearchVisibilityFeatureConfig team3member Public.FeatureStatusDisabled

    setTeamSearchVisibilityInternal tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibility tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibilityInternal tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibilityFeatureConfig team3member Public.FeatureStatusEnabled

getClassifiedDomains ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
  UserId ->
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains ->
  m ()
getClassifiedDomains member tid =
  assertFlagWithConfig @Public.TeamFeatureClassifiedDomainsConfig $
    Util.getTeamFeatureFlag Public.TeamFeatureClassifiedDomains member tid

getClassifiedDomainsInternal ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains ->
  m ()
getClassifiedDomainsInternal tid =
  assertFlagWithConfig @Public.TeamFeatureClassifiedDomainsConfig $
    Util.getTeamFeatureFlagInternal Public.TeamFeatureClassifiedDomains tid

testClassifiedDomainsEnabled :: TestM ()
testClassifiedDomainsEnabled = do
  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  let expected =
        Public.TeamFeatureStatusWithConfig
          { Public.tfwcStatus = Public.FeatureStatusEnabled,
            Public.tfwcConfig = Public.TeamFeatureClassifiedDomainsConfig [Domain "example.com"]
          }

  let getClassifiedDomainsFeatureConfig ::
        (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
        UserId ->
        Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains ->
        m ()
      getClassifiedDomainsFeatureConfig uid = do
        assertFlagWithConfig @Public.TeamFeatureClassifiedDomainsConfig $
          Util.getFeatureConfig Public.TeamFeatureClassifiedDomains uid

  getClassifiedDomains member tid expected
  getClassifiedDomainsInternal tid expected
  getClassifiedDomainsFeatureConfig member expected

testClassifiedDomainsDisabled :: TestM ()
testClassifiedDomainsDisabled = do
  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  let expected =
        Public.TeamFeatureStatusWithConfig
          { Public.tfwcStatus = Public.FeatureStatusDisabled,
            Public.tfwcConfig = Public.TeamFeatureClassifiedDomainsConfig []
          }

  let getClassifiedDomainsFeatureConfig ::
        (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
        UserId ->
        Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains ->
        m ()
      getClassifiedDomainsFeatureConfig uid = do
        assertFlagWithConfig @Public.TeamFeatureClassifiedDomainsConfig $
          Util.getFeatureConfig Public.TeamFeatureClassifiedDomains uid

  let classifiedDomainsDisabled = \opts ->
        opts
          & over
            (optSettings . setFeatureFlags . flagClassifiedDomains)
            (\s -> s {Public.tfwcStatus = Public.FeatureStatusDisabled})
  withSettingsOverrides classifiedDomainsDisabled $ do
    getClassifiedDomains member tid expected
    getClassifiedDomainsInternal tid expected
    getClassifiedDomainsFeatureConfig member expected

testSimpleFlag ::
  forall (a :: Public.FeatureTag).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a),
    ToJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
  ) =>
  Public.FeatureStatus ->
  TestM ()
testSimpleFlag defaultValue = do
  let feature = Public.knownTeamFeatureName @a
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getFlag :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFlag expected =
        flip (assertFlagNoConfig @a) expected $ Util.getTeamFeatureFlag feature member tid

      getFeatureConfig :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFeatureConfig expected =
        flip (assertFlagNoConfig @a) expected $ Util.getFeatureConfig feature member

      getFlagInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @a) expected $ Util.getTeamFeatureFlagInternal feature tid

      setFlagInternal :: Public.FeatureStatus -> TestM ()
      setFlagInternal statusValue =
        void $ Util.putTeamFeatureFlagInternal @a expect2xx tid (Public.TeamFeatureStatusNoConfig statusValue)

  assertFlagForbidden $ Util.getTeamFeatureFlag feature nonMember tid

  let otherValue = case defaultValue of
        Public.FeatureStatusDisabled -> Public.FeatureStatusEnabled
        Public.FeatureStatusEnabled -> Public.FeatureStatusDisabled

  -- Initial value should be the default value
  getFlag defaultValue
  getFlagInternal defaultValue
  getFeatureConfig defaultValue

  -- Setting should work
  cannon <- view tsCannon
  -- should receive an event
  WS.bracketR cannon member $ \ws -> do
    setFlagInternal otherValue
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigUpdate feature otherValue
  getFlag otherValue
  getFeatureConfig otherValue
  getFlagInternal otherValue

  -- Clean up
  setFlagInternal defaultValue
  getFlag defaultValue

testSimpleFlagWithLockStatus ::
  forall (a :: Public.FeatureTag).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig 'Public.WithLockStatus a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithLockStatus a),
    ToJSON (Public.TeamFeatureStatus 'Public.WithLockStatus a)
  ) =>
  Public.FeatureStatus ->
  Public.LockStatusValue ->
  TestM ()
testSimpleFlagWithLockStatus defaultStatus defaultLockStatus = do
  galley <- view tsGalley
  let feature = Public.knownTeamFeatureName @a
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing
  let getFlag :: HasCallStack => Public.FeatureStatus -> Public.LockStatusValue -> TestM ()
      getFlag expectedStatus expectedLockStatus = do
        let flag = Util.getTeamFeatureFlag feature member tid
        assertFlagNoConfigWithLockStatus @a flag expectedStatus expectedLockStatus

      getFeatureConfig :: HasCallStack => Public.FeatureStatus -> Public.LockStatusValue -> TestM ()
      getFeatureConfig expectedStatus expectedLockStatus = do
        let flag = Util.getFeatureConfig feature member
        assertFlagNoConfigWithLockStatus @a flag expectedStatus expectedLockStatus

      getFlagInternal :: HasCallStack => Public.FeatureStatus -> Public.LockStatusValue -> TestM ()
      getFlagInternal expectedStatus expectedLockStatus = do
        let flag = Util.getTeamFeatureFlagInternal feature tid
        assertFlagNoConfigWithLockStatus @a flag expectedStatus expectedLockStatus

      getFlags expectedStatus expectedLockStatus = do
        getFlag expectedStatus expectedLockStatus
        getFeatureConfig expectedStatus expectedLockStatus
        getFlagInternal expectedStatus expectedLockStatus

      setFlagWithGalley :: Public.FeatureStatus -> TestM ()
      setFlagWithGalley statusValue =
        Util.putTeamFeatureFlagWithGalley @a galley owner tid (Public.TeamFeatureStatusNoConfig statusValue)
          !!! statusCode === const 200

      assertSetStatusForbidden :: Public.FeatureStatus -> TestM ()
      assertSetStatusForbidden statusValue =
        Util.putTeamFeatureFlagWithGalley @a galley owner tid (Public.TeamFeatureStatusNoConfig statusValue)
          !!! statusCode === const 409

      setLockStatus :: Public.LockStatusValue -> TestM ()
      setLockStatus lockStatus =
        Util.setLockStatusInternal @a galley tid lockStatus
          !!! statusCode === const 200

  assertFlagForbidden $ Util.getTeamFeatureFlag feature nonMember tid

  let otherStatus = case defaultStatus of
        Public.FeatureStatusDisabled -> Public.FeatureStatusEnabled
        Public.FeatureStatusEnabled -> Public.FeatureStatusDisabled

  -- Initial status and lock status should be the defaults
  getFlags defaultStatus defaultLockStatus

  -- unlock feature if it is locked
  when (defaultLockStatus == Public.Locked) $ setLockStatus Public.Unlocked

  -- setting should work
  cannon <- view tsCannon
  -- should receive an event
  WS.bracketR cannon member $ \ws -> do
    setFlagWithGalley otherStatus
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigWithLockStatusUpdate feature otherStatus Public.Unlocked

  getFlags otherStatus Public.Unlocked

  -- lock feature
  setLockStatus Public.Locked
  -- feature status should now be the default again
  getFlags defaultStatus Public.Locked
  assertSetStatusForbidden defaultStatus
  -- unlock feature
  setLockStatus Public.Unlocked
  -- feature status should be the previously set value
  getFlags otherStatus Public.Unlocked

  -- clean up
  setFlagWithGalley defaultStatus
  setLockStatus defaultLockStatus
  getFlags defaultStatus defaultLockStatus

testSelfDeletingMessages :: TestM ()
testSelfDeletingMessages = do
  defLockStatus :: Public.LockStatusValue <-
    view
      ( tsGConf
          . optSettings
          . setFeatureFlags
          . flagSelfDeletingMessages
          . unDefaults
          . to Public.tfwcapsLockStatus
      )

  -- personal users
  let settingWithoutLockStatus :: FeatureStatus -> Int32 -> Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSelfDeletingMessages
      settingWithoutLockStatus stat tout =
        Public.TeamFeatureStatusWithConfig @Public.TeamFeatureSelfDeletingMessagesConfig
          stat
          (Public.TeamFeatureSelfDeletingMessagesConfig tout)
      settingWithLockStatus :: FeatureStatus -> Int32 -> Public.LockStatusValue -> Public.TeamFeatureStatus 'Public.WithLockStatus 'Public.TeamFeatureSelfDeletingMessages
      settingWithLockStatus stat tout lockStatus =
        Public.TeamFeatureStatusWithConfigAndLockStatus @Public.TeamFeatureSelfDeletingMessagesConfig
          stat
          (Public.TeamFeatureSelfDeletingMessagesConfig tout)
          lockStatus

  personalUser <- Util.randomUser
  Util.getFeatureConfig Public.TeamFeatureSelfDeletingMessages personalUser
    !!! responseJsonEither === const (Right $ settingWithLockStatus FeatureStatusEnabled 0 defLockStatus)

  -- team users
  galley <- view tsGalley
  (owner, tid, []) <- Util.createBindingTeamWithNMembers 0

  let checkSet :: FeatureStatus -> Int32 -> Int -> TestM ()
      checkSet stat tout expectedStatusCode =
        do
          Util.putTeamFeatureFlagInternal @'Public.TeamFeatureSelfDeletingMessages
            galley
            tid
            (settingWithoutLockStatus stat tout)
          !!! statusCode === const expectedStatusCode

      -- internal, public (/team/:tid/features), and team-agnostic (/feature-configs).
      checkGet :: HasCallStack => FeatureStatus -> Int32 -> Public.LockStatusValue -> TestM ()
      checkGet stat tout lockStatus = do
        let expected = settingWithLockStatus stat tout lockStatus
        forM_
          [ Util.getTeamFeatureFlagInternal Public.TeamFeatureSelfDeletingMessages tid,
            Util.getTeamFeatureFlagWithGalley Public.TeamFeatureSelfDeletingMessages galley owner tid,
            Util.getFeatureConfig Public.TeamFeatureSelfDeletingMessages owner
          ]
          (!!! responseJsonEither === const (Right expected))

      checkSetLockStatus :: HasCallStack => Public.LockStatusValue -> TestM ()
      checkSetLockStatus status =
        do
          Util.setLockStatusInternal @'Public.TeamFeatureSelfDeletingMessages galley tid status
          !!! statusCode === const 200

  -- test that the default lock status comes from `galley.yaml`.
  -- use this to change `galley.integration.yaml` locally and manually test that conf file
  -- parsing works as expected.
  checkGet FeatureStatusEnabled 0 defLockStatus

  case defLockStatus of
    Public.Locked -> do
      checkSet FeatureStatusDisabled 0 409
    Public.Unlocked -> do
      checkSet FeatureStatusDisabled 0 200
      checkGet FeatureStatusDisabled 0 Public.Unlocked
      checkSet FeatureStatusEnabled 0 200
      checkGet FeatureStatusEnabled 0 Public.Unlocked

  -- now don't worry about what's in the config, write something to cassandra, and test with that.
  checkSetLockStatus Public.Locked
  checkGet FeatureStatusEnabled 0 Public.Locked
  checkSet FeatureStatusDisabled 0 409
  checkGet FeatureStatusEnabled 0 Public.Locked
  checkSet FeatureStatusEnabled 30 409
  checkGet FeatureStatusEnabled 0 Public.Locked
  checkSetLockStatus Public.Unlocked
  checkGet FeatureStatusEnabled 0 Public.Unlocked
  checkSet FeatureStatusDisabled 0 200
  checkGet FeatureStatusDisabled 0 Public.Unlocked
  checkSet FeatureStatusEnabled 30 200
  checkGet FeatureStatusEnabled 30 Public.Unlocked
  checkSet FeatureStatusDisabled 30 200
  checkGet FeatureStatusDisabled 30 Public.Unlocked
  checkSetLockStatus Public.Locked
  checkGet FeatureStatusEnabled 0 Public.Locked
  checkSet FeatureStatusEnabled 50 409
  checkSetLockStatus Public.Unlocked
  checkGet FeatureStatusDisabled 30 Public.Unlocked

testGuestLinksInternal :: TestM ()
testGuestLinksInternal = do
  galley <- view tsGalley
  testGuestLinks
    (const $ Util.getTeamFeatureFlagInternal Public.TeamFeatureGuestLinks)
    (const $ Util.putTeamFeatureFlagInternal @'Public.TeamFeatureGuestLinks galley)
    (Util.setLockStatusInternal @'Public.TeamFeatureGuestLinks galley)

testGuestLinksPublic :: TestM ()
testGuestLinksPublic = do
  galley <- view tsGalley
  testGuestLinks
    (Util.getTeamFeatureFlagWithGalley Public.TeamFeatureGuestLinks galley)
    (Util.putTeamFeatureFlagWithGalley @'Public.TeamFeatureGuestLinks galley)
    (Util.setLockStatusInternal @'Public.TeamFeatureGuestLinks galley)

testGuestLinks ::
  (UserId -> TeamId -> TestM ResponseLBS) ->
  (UserId -> TeamId -> Public.TeamFeatureStatusNoConfig -> TestM ResponseLBS) ->
  (TeamId -> Public.LockStatusValue -> TestM ResponseLBS) ->
  TestM ()
testGuestLinks getStatus putStatus setLockStatusInternal = do
  (owner, tid, []) <- Util.createBindingTeamWithNMembers 0
  let checkGet :: HasCallStack => Public.FeatureStatus -> Public.LockStatusValue -> TestM ()
      checkGet status lock =
        getStatus owner tid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfigAndLockStatus status lock))

      checkSet :: HasCallStack => Public.FeatureStatus -> Int -> TestM ()
      checkSet status expectedStatusCode =
        putStatus owner tid (Public.TeamFeatureStatusNoConfig status) !!! statusCode === const expectedStatusCode

      checkSetLockStatusInternal :: HasCallStack => Public.LockStatusValue -> TestM ()
      checkSetLockStatusInternal lockStatus =
        setLockStatusInternal tid lockStatus !!! statusCode === const 200

  checkGet Public.FeatureStatusEnabled Public.Unlocked
  checkSet Public.FeatureStatusDisabled 200
  checkGet Public.FeatureStatusDisabled Public.Unlocked
  checkSet Public.FeatureStatusEnabled 200
  checkGet Public.FeatureStatusEnabled Public.Unlocked
  checkSet Public.FeatureStatusDisabled 200
  checkGet Public.FeatureStatusDisabled Public.Unlocked
  -- when locks status is locked the team default feature status should be returned
  -- and the team feature status can not be changed
  checkSetLockStatusInternal Public.Locked
  checkGet Public.FeatureStatusEnabled Public.Locked
  checkSet Public.FeatureStatusDisabled 409
  -- when lock status is unlocked again the previously set feature status is restored
  checkSetLockStatusInternal Public.Unlocked
  checkGet Public.FeatureStatusDisabled Public.Unlocked

-- | Call 'GET /teams/:tid/features' and 'GET /feature-configs', and check if all
-- features are there.
testAllFeatures :: TestM ()
testAllFeatures = do
  defLockStatus :: Public.LockStatusValue <-
    view
      ( tsGConf
          . optSettings
          . setFeatureFlags
          . flagSelfDeletingMessages
          . unDefaults
          . to Public.tfwcapsLockStatus
      )

  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  Util.getAllTeamFeatures member tid !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by default in galley -}))
  Util.getAllTeamFeaturesPersonal member !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by default in galley -}))

  randomPersonalUser <- Util.randomUser
  Util.getAllTeamFeaturesPersonal randomPersonalUser !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by 'getAfcConferenceCallingDefNew' in brig -}))
  where
    expected confCalling lockState =
      object
        [ toS TeamFeatureLegalHold .= Public.TeamFeatureStatusNoConfig FeatureStatusDisabled,
          toS TeamFeatureSSO .= Public.TeamFeatureStatusNoConfig FeatureStatusDisabled,
          toS TeamFeatureSearchVisibility .= Public.TeamFeatureStatusNoConfig FeatureStatusDisabled,
          toS TeamFeatureValidateSAMLEmails .= Public.TeamFeatureStatusNoConfig FeatureStatusDisabled,
          toS TeamFeatureDigitalSignatures .= Public.TeamFeatureStatusNoConfig FeatureStatusDisabled,
          toS TeamFeatureAppLock
            .= Public.TeamFeatureStatusWithConfig
              FeatureStatusEnabled
              (Public.TeamFeatureAppLockConfig (Public.EnforceAppLock False) (60 :: Int32)),
          toS TeamFeatureFileSharing .= Public.TeamFeatureStatusNoConfigAndLockStatus FeatureStatusEnabled Public.Unlocked,
          toS TeamFeatureClassifiedDomains
            .= Public.TeamFeatureStatusWithConfig
              FeatureStatusEnabled
              (Public.TeamFeatureClassifiedDomainsConfig [Domain "example.com"]),
          toS TeamFeatureConferenceCalling
            .= Public.TeamFeatureStatusNoConfig confCalling,
          toS TeamFeatureSelfDeletingMessages
            .= Public.TeamFeatureStatusWithConfigAndLockStatus @Public.TeamFeatureSelfDeletingMessagesConfig
              FeatureStatusEnabled
              (Public.TeamFeatureSelfDeletingMessagesConfig 0)
              lockState,
          toS TeamFeatureGuestLinks
            .= Public.TeamFeatureStatusNoConfigAndLockStatus
              FeatureStatusEnabled
              Public.Unlocked,
          toS TeamFeatureValidateSAMLEmails .= Public.TeamFeatureStatusNoConfig FeatureStatusEnabled,
          toS TeamFeatureGuestLinks .= Public.TeamFeatureStatusNoConfigAndLockStatus FeatureStatusEnabled Public.Unlocked,
          toS TeamFeatureSndFactorPasswordChallenge .= Public.TeamFeatureStatusNoConfigAndLockStatus FeatureStatusDisabled Public.Locked
        ]
    toS :: FeatureTag -> Aeson.Key
    toS = AesonKey.fromText . TE.decodeUtf8 . toByteString'

testFeatureConfigConsistency :: TestM ()
testFeatureConfigConsistency = do
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  allFeaturesRes <- Util.getAllFeatureConfigs member >>= parseObjectKeys

  allTeamFeaturesRes <- Util.getAllTeamFeatures member tid >>= parseObjectKeys

  unless (allTeamFeaturesRes `Set.isSubsetOf` allFeaturesRes) $
    liftIO $ expectationFailure (show allTeamFeaturesRes <> " is not a subset of " <> show allFeaturesRes)
  where
    parseObjectKeys :: ResponseLBS -> TestM (Set.Set Text)
    parseObjectKeys res = do
      case responseJsonEither res of
        Left err -> liftIO $ assertFailure ("Did not parse as an object" <> err)
        Right (val :: Aeson.Value) ->
          case val of
            (Aeson.Object hm) -> pure (Set.fromList . map AesonKey.toText . KeyMap.keys $ hm)
            x -> liftIO $ assertFailure ("JSON was not an object, but " <> show x)

testSearchVisibilityInbound :: TestM ()
testSearchVisibilityInbound = do
  let defaultValue = FeatureStatusDisabled
  let feature = Public.knownTeamFeatureName @'TeamFeatureSearchVisibilityInbound
  owner <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []

  let getFlagInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @'TeamFeatureSearchVisibilityInbound) expected $ Util.getTeamFeatureFlagInternal feature tid

      setFlagInternal :: Public.FeatureStatus -> TestM ()
      setFlagInternal statusValue =
        void $ Util.putTeamFeatureFlagInternal @'TeamFeatureSearchVisibilityInbound expect2xx tid (Public.TeamFeatureStatusNoConfig statusValue)

  let otherValue = case defaultValue of
        Public.FeatureStatusDisabled -> Public.FeatureStatusEnabled
        Public.FeatureStatusEnabled -> Public.FeatureStatusDisabled

  -- Initial value should be the default value
  getFlagInternal defaultValue
  setFlagInternal otherValue
  getFlagInternal otherValue

testFeatureNoConfigMultiSearchVisibilityInbound :: TestM ()
testFeatureNoConfigMultiSearchVisibilityInbound = do
  owner1 <- Util.randomUser
  team1 <- Util.createNonBindingTeam "team1" owner1 []

  owner2 <- Util.randomUser
  team2 <- Util.createNonBindingTeam "team2" owner2 []

  let setFlagInternal :: TeamId -> Public.FeatureStatus -> TestM ()
      setFlagInternal tid statusValue =
        void $ Util.putTeamFeatureFlagInternal @'TeamFeatureSearchVisibilityInbound expect2xx tid (Public.TeamFeatureStatusNoConfig statusValue)

  setFlagInternal team2 Public.FeatureStatusEnabled

  r <-
    getFeatureStatusMulti TeamFeatureSearchVisibilityInbound (Multi.TeamFeatureNoConfigMultiRequest [team1, team2])
      <!! statusCode === const 200

  Multi.TeamFeatureNoConfigMultiResponse teamsStatuses :: Multi.TeamFeatureNoConfigMultiResponse 'TeamFeatureSearchVisibilityInbound <- responseJsonError r

  liftIO $ do
    length teamsStatuses @?= 2

    Multi.TeamStatus _ team1Status team1WriteTime <- Util.assertOne (filter ((== team1) . Multi.team) teamsStatuses)
    team1Status @?= Public.FeatureStatusDisabled
    assertBool "expected Nothing" (isNothing team1WriteTime)

    Multi.TeamStatus _ team2Status team2WriteTime <- Util.assertOne (filter ((== team2) . Multi.team) teamsStatuses)
    team2Status @?= Public.FeatureStatusEnabled
    assertBool "expected Just" (isJust team2WriteTime)

assertFlagForbidden :: HasCallStack => TestM ResponseLBS -> TestM ()
assertFlagForbidden res = do
  res !!! do
    statusCode === const 403
    fmap label . responseJsonMaybe === const (Just "no-team-member")

assertFlagNoConfig ::
  forall (a :: Public.FeatureTag).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a),
    Public.KnownTeamFeatureName a
  ) =>
  TestM ResponseLBS ->
  Public.FeatureStatus ->
  TestM ()
assertFlagNoConfig res expected = do
  res !!! do
    statusCode === const 200
    ( fmap Public.tfwoStatus
        . responseJsonEither @(Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
      )
      === const (Right expected)

assertFlagNoConfigWithLockStatus ::
  forall (a :: Public.FeatureTag).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig 'Public.WithLockStatus a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithLockStatus a),
    Public.KnownTeamFeatureName a
  ) =>
  TestM ResponseLBS ->
  Public.FeatureStatus ->
  Public.LockStatusValue ->
  TestM ()
assertFlagNoConfigWithLockStatus res expectedStatus expectedLockStatus = do
  res !!! do
    statusCode === const 200
    responseJsonEither @(Public.TeamFeatureStatus 'Public.WithLockStatus a)
      === const (Right (Public.TeamFeatureStatusNoConfigAndLockStatus expectedStatus expectedLockStatus))

assertFlagWithConfig ::
  forall cfg m.
  ( HasCallStack,
    Eq cfg,
    ToSchema cfg,
    Show cfg,
    Typeable cfg,
    MonadIO m,
    MonadCatch m
  ) =>
  m ResponseLBS ->
  Public.TeamFeatureStatusWithConfig cfg ->
  m ()
assertFlagWithConfig response expected = do
  r <- response
  let rJson = responseJsonEither @(Public.TeamFeatureStatusWithConfig cfg) r
  pure r !!! statusCode === const 200
  liftIO $ do
    fmap Public.tfwcStatus rJson @?= (Right . Public.tfwcStatus $ expected)
    fmap Public.tfwcConfig rJson @?= (Right . Public.tfwcConfig $ expected)

wsAssertFeatureConfigUpdate :: Public.FeatureTag -> Public.FeatureStatus -> Notification -> IO ()
wsAssertFeatureConfigUpdate teamFeature status notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= teamFeature
  FeatureConfig._eventData e @?= EdFeatureWithoutConfigChanged (Public.TeamFeatureStatusNoConfig status)

wsAssertFeatureConfigWithLockStatusUpdate :: Public.FeatureTag -> Public.FeatureStatus -> Public.LockStatusValue -> Notification -> IO ()
wsAssertFeatureConfigWithLockStatusUpdate teamFeature status lockStatus notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= teamFeature
  FeatureConfig._eventData e
    @?= EdFeatureWithoutConfigAndLockStatusChanged
      (Public.TeamFeatureStatusNoConfigAndLockStatus status lockStatus)
