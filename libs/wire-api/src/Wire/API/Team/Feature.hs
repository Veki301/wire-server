{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.Team.Feature
  (
  )
where

-- TeamFeatureName (..),
-- TeamFeatureStatus,
-- TeamFeatureAppLockConfig (..),
-- SelfDeletingMessagesConfig (..),
-- TeamFeatureClassifiedDomainsConfig (..),
-- FeatureStatus (..),
-- FeatureHasNoConfig,
-- EnforceAppLock (..),
-- KnownTeamFeatureName (..),
-- TeamFeatureStatusNoConfig (..),
-- TeamFeatureStatusNoConfigAndLockStatus (..),
-- TeamFeatureStatusWithConfig (..),
-- TeamFeatureStatusWithConfigAndLockStatus (..),
-- HasDeprecatedFeatureName (..),
-- AllFeatureConfigs (..),
-- LockStatus (..),
-- LockStatusValue (..),
-- IncludeLockStatus (..),
-- DefTeamFeatureStatus (..),

-- -- * Swagger
-- typeTeamFeatureName,
-- typeFeatureStatus,
-- modelTeamFeatureStatusNoConfig,
-- modelTeamFeatureStatusWithConfig,
-- modelTeamFeatureAppLockConfig,
-- modelTeamFeatureClassifiedDomainsConfig,
-- modelSelfDeletingMessagesConfig,
-- modelTeamFeatureStatusWithConfigAndLockStatus,
-- modelTeamFeatureStatusNoConfigAndLockStatus,
-- modelForTeamFeature,
-- modelLockStatus,

import qualified Cassandra.CQL as Cass
import qualified Data.Attoparsec.ByteString as Parser
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..), fromByteString, toByteString')
import Data.Domain (Domain)
import Data.Either.Extra (maybeToEither)
import Data.Kind (Constraint)
import Data.Proxy
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Deriving.Aeson
import GHC.TypeLits
import Imports
import Servant (FromHttpApiData (..))
import Test.QuickCheck.Arbitrary (arbitrary)
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

----------------------------------------------------------------------
-- TeamFeatureName

-- | If you add a constructor here, you need extend multiple definitions, which
--   aren't checked by GHC.
--
--   Follow this Checklist:
--
-- * libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs
--   * add call to 'testRoundTrip'
-- * libs/wire-api/src/Wire/API/Routes/Public/Galley.hs
--   * add a FeatureStatusGet (and maybe FeatureStatusPut) route to the FeatureAPI
--   * maybe add a FeatureConfigGet route to FeatureAPI
-- * services/galley/src/Galley/API/Internal.hs
--   * add IFeatureStatus to IFeatureAPI
-- * libs/galley-types/src/Galley/Types/Teams.hs
--   * FeatureFlags for server config file
--   * Update the Arbitrary instance of FeatureFlags
--       in libs/galley-types/test/unit/Test/Galley/Types.hs
--   * roleHiddenPermissions ChangeTeamFeature and ViewTeamFeature
-- * add the feature status to `AllFeatureConfigs` (see below)
--   * follow the type errors and fix them (e.g. in services/galley/src/Galley/API/Teams/Features.hs)
-- * services/galley/schema/src/
--   * add a migration like the one in "V43_TeamFeatureDigitalSignatures.hs"
-- * services/galley/test/integration/API/Teams/Feature.hs
--   * add an integration test for the feature
--   * extend testAllFeatures
-- * consider personal-account configurability (like for `conferenceCalling`, see
--     eg. https://github.com/wireapp/wire-server/pull/1811,
--     https://github.com/wireapp/wire-server/pull/1818)
--
-- An example of all the places to change (including compiler errors and failing tests) can be found
-- in eg. https://github.com/wireapp/wire-server/pull/1652.  (applock and conference calling also
-- add interesting aspects, though.)
--
-- Using something like '[minBound..]' on those expressions would require dependent types.  We
-- could generate exhaustive lists of those calls using TH, along the lines of:
--
-- @
-- forAllTeamFeatureNames ::
--   ExpQ {- [forall (a :: TeamFeatureName). b] -} ->
--   ExpQ {- [b] -}
-- forAllTeamFeatureNames =
--   error
--     "...  and then somehow turn the values from '[minBound..]' into \
--     \type applications in the syntax tree"
-- @
--
-- But that seems excessive.  Let's wait for dependent types to be ready in ghc!
data TeamFeatureName
  = TeamFeatureLegalHold
  | TeamFeatureSSO
  | TeamFeatureSearchVisibility
  | TeamFeatureValidateSAMLEmails
  | TeamFeatureDigitalSignatures
  | TeamFeatureAppLock
  | TeamFeatureFileSharing
  | TeamFeatureClassifiedDomains
  | TeamFeatureConferenceCalling
  | TeamFeatureSelfDeletingMessages
  | TeamFeatureGuestLinks
  | TeamFeatureSndFactorPasswordChallenge
  | TeamFeatureSearchVisibilityInbound
  deriving stock (Eq, Show, Ord, Generic, Enum, Bounded, Typeable)
  deriving (Arbitrary) via (GenericUniform TeamFeatureName)

class KnownTeamFeatureName (a :: TeamFeatureName) where
  knownTeamFeatureName :: TeamFeatureName
  type KnownTeamFeatureNameSymbol a :: Symbol

instance KnownTeamFeatureName 'TeamFeatureLegalHold where
  type KnownTeamFeatureNameSymbol 'TeamFeatureLegalHold = "legalhold"
  knownTeamFeatureName = TeamFeatureLegalHold

instance KnownTeamFeatureName 'TeamFeatureSSO where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSSO = "sso"
  knownTeamFeatureName = TeamFeatureSSO

instance KnownTeamFeatureName 'TeamFeatureSearchVisibility where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSearchVisibility = "searchVisibility"
  knownTeamFeatureName = TeamFeatureSearchVisibility

instance KnownTeamFeatureName 'TeamFeatureValidateSAMLEmails where
  type KnownTeamFeatureNameSymbol 'TeamFeatureValidateSAMLEmails = "validateSAMLemails"
  knownTeamFeatureName = TeamFeatureValidateSAMLEmails

instance KnownTeamFeatureName 'TeamFeatureDigitalSignatures where
  type KnownTeamFeatureNameSymbol 'TeamFeatureDigitalSignatures = "digitalSignatures"
  knownTeamFeatureName = TeamFeatureDigitalSignatures

instance KnownTeamFeatureName 'TeamFeatureAppLock where
  type KnownTeamFeatureNameSymbol 'TeamFeatureAppLock = "appLock"
  knownTeamFeatureName = TeamFeatureAppLock

instance KnownTeamFeatureName 'TeamFeatureFileSharing where
  type KnownTeamFeatureNameSymbol 'TeamFeatureFileSharing = "fileSharing"
  knownTeamFeatureName = TeamFeatureFileSharing

instance KnownTeamFeatureName 'TeamFeatureClassifiedDomains where
  type KnownTeamFeatureNameSymbol 'TeamFeatureClassifiedDomains = "classifiedDomains"
  knownTeamFeatureName = TeamFeatureClassifiedDomains

instance KnownTeamFeatureName 'TeamFeatureConferenceCalling where
  type KnownTeamFeatureNameSymbol 'TeamFeatureConferenceCalling = "conferenceCalling"
  knownTeamFeatureName = TeamFeatureConferenceCalling

instance KnownTeamFeatureName 'TeamFeatureSelfDeletingMessages where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSelfDeletingMessages = "selfDeletingMessages"
  knownTeamFeatureName = TeamFeatureSelfDeletingMessages

instance KnownTeamFeatureName 'TeamFeatureGuestLinks where
  type KnownTeamFeatureNameSymbol 'TeamFeatureGuestLinks = "conversationGuestLinks"
  knownTeamFeatureName = TeamFeatureGuestLinks

instance KnownTeamFeatureName 'TeamFeatureSndFactorPasswordChallenge where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSndFactorPasswordChallenge = "sndFactorPasswordChallenge"
  knownTeamFeatureName = TeamFeatureSndFactorPasswordChallenge

instance KnownTeamFeatureName 'TeamFeatureSearchVisibilityInbound where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSearchVisibilityInbound = "searchVisibilityInbound"
  knownTeamFeatureName = TeamFeatureSearchVisibilityInbound

instance FromByteString TeamFeatureName where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Left e -> fail $ "Invalid TeamFeatureName: " <> show e
        Right "legalhold" -> pure TeamFeatureLegalHold
        Right "sso" -> pure TeamFeatureSSO
        Right "searchVisibility" -> pure TeamFeatureSearchVisibility
        Right "search-visibility" -> pure TeamFeatureSearchVisibility
        Right "validateSAMLemails" -> pure TeamFeatureValidateSAMLEmails
        Right "validate-saml-emails" -> pure TeamFeatureValidateSAMLEmails
        Right "digitalSignatures" -> pure TeamFeatureDigitalSignatures
        Right "digital-signatures" -> pure TeamFeatureDigitalSignatures
        Right "appLock" -> pure TeamFeatureAppLock
        Right "fileSharing" -> pure TeamFeatureFileSharing
        Right "classifiedDomains" -> pure TeamFeatureClassifiedDomains
        Right "conferenceCalling" -> pure TeamFeatureConferenceCalling
        Right "selfDeletingMessages" -> pure TeamFeatureSelfDeletingMessages
        Right "conversationGuestLinks" -> pure TeamFeatureGuestLinks
        Right "sndFactorPasswordChallenge" -> pure TeamFeatureSndFactorPasswordChallenge
        Right "searchVisibilityInbound" -> pure TeamFeatureSearchVisibilityInbound
        Right t -> fail $ "Invalid TeamFeatureName: " <> T.unpack t

-- TODO: how do we make this consistent with 'KnownTeamFeatureNameSymbol'?  add a test for
-- that?  anyway do we really need both?
instance ToByteString TeamFeatureName where
  builder TeamFeatureLegalHold = "legalhold"
  builder TeamFeatureSSO = "sso"
  builder TeamFeatureSearchVisibility = "searchVisibility"
  builder TeamFeatureValidateSAMLEmails = "validateSAMLemails"
  builder TeamFeatureDigitalSignatures = "digitalSignatures"
  builder TeamFeatureAppLock = "appLock"
  builder TeamFeatureFileSharing = "fileSharing"
  builder TeamFeatureClassifiedDomains = "classifiedDomains"
  builder TeamFeatureConferenceCalling = "conferenceCalling"
  builder TeamFeatureSelfDeletingMessages = "selfDeletingMessages"
  builder TeamFeatureGuestLinks = "conversationGuestLinks"
  builder TeamFeatureSndFactorPasswordChallenge = "sndFactorPasswordChallenge"
  builder TeamFeatureSearchVisibilityInbound = "searchVisibilityInbound"

instance ToSchema TeamFeatureName where
  schema =
    enum @Text
      "TeamFeatureName"
      $ mconcat
        (map (\feat -> element (cs . toByteString' $ feat) feat) [minBound .. maxBound])

class HasDeprecatedFeatureName (a :: TeamFeatureName) where
  type DeprecatedFeatureName a :: Symbol

instance HasDeprecatedFeatureName 'TeamFeatureSearchVisibility where
  type DeprecatedFeatureName 'TeamFeatureSearchVisibility = "search-visibility"

instance HasDeprecatedFeatureName 'TeamFeatureValidateSAMLEmails where
  type DeprecatedFeatureName 'TeamFeatureValidateSAMLEmails = "validate-saml-emails"

instance HasDeprecatedFeatureName 'TeamFeatureDigitalSignatures where
  type DeprecatedFeatureName 'TeamFeatureDigitalSignatures = "digital-signatures"

typeTeamFeatureName :: Doc.DataType
typeTeamFeatureName = Doc.string . Doc.enum $ cs . toByteString' <$> [(minBound :: TeamFeatureName) ..]

----------------------------------------------------------------------
-- FeatureStatus

data FeatureStatus
  = FeatureStatusEnabled
  | FeatureStatusDisabled
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving (Arbitrary) via (GenericUniform FeatureStatus)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema FeatureStatus)

typeFeatureStatus :: Doc.DataType
typeFeatureStatus =
  Doc.string $
    Doc.enum
      [ "enabled",
        "disabled"
      ]

instance ToSchema FeatureStatus where
  schema =
    enum @Text "FeatureStatus" $
      mconcat
        [ element "enabled" FeatureStatusEnabled,
          element "disabled" FeatureStatusDisabled
        ]

instance ToByteString FeatureStatus where
  builder FeatureStatusEnabled = "enabled"
  builder FeatureStatusDisabled = "disabled"

instance FromByteString FeatureStatus where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Right "enabled" -> pure FeatureStatusEnabled
        Right "disabled" -> pure FeatureStatusDisabled
        Right t -> fail $ "Invalid FeatureStatus: " <> T.unpack t
        Left e -> fail $ "Invalid FeatureStatus: " <> show e

instance Cass.Cql FeatureStatus where
  ctype = Cass.Tagged Cass.IntColumn

  fromCql (Cass.CqlInt n) = case n of
    0 -> pure FeatureStatusDisabled
    1 -> pure FeatureStatusEnabled
    _ -> Left "fromCql: Invalid FeatureStatus"
  fromCql _ = Left "fromCql: FeatureStatus: CqlInt expected"

  toCql FeatureStatusDisabled = Cass.CqlInt 0
  toCql FeatureStatusEnabled = Cass.CqlInt 1

--------------------------------------------------------------------------------
-- IsFeature type class

class IsFeatureConfig cfg where
  type FeatureSymbol cfg :: Symbol
  defFeatureStatus :: WithStatus cfg

instance IsFeatureConfig GuestLinksConfig where
  type FeatureSymbol GuestLinksConfig = "conversationGuestLinks"
  defFeatureStatus = WithStatus FeatureStatusEnabled LockStatusUnlocked GuestLinksConfig

instance IsFeatureConfig ValidateSAMLEmailsConfig where
  type FeatureSymbol ValidateSAMLEmailsConfig = "validateSAMLemails"
  defFeatureStatus = WithStatus FeatureStatusEnabled LockStatusUnlocked ValidateSAMLEmailsConfig

instance IsFeatureConfig SndFactorPasswordChallengeConfig where
  type FeatureSymbol SndFactorPasswordChallengeConfig = "sndFactorPasswordChallenge"
  defFeatureStatus = WithStatus FeatureStatusDisabled LockStatusLocked SndFactorPasswordChallengeConfig

instance IsFeatureConfig SearchVisibilityInboundConfig where
  type FeatureSymbol SearchVisibilityInboundConfig = "searchVisibilityInbound"
  defFeatureStatus = WithStatus FeatureStatusDisabled LockStatusUnlocked SearchVisibilityInboundConfig

instance IsFeatureConfig FileSharingConfig where
  type FeatureSymbol FileSharingConfig = "fileSharing"
  defFeatureStatus = WithStatus FeatureStatusEnabled LockStatusUnlocked FileSharingConfig

instance IsFeatureConfig SelfDeletingMessagesConfig where
  type FeatureSymbol SelfDeletingMessagesConfig = "selfDeletingMessages"
  defFeatureStatus =
    WithStatus
      FeatureStatusEnabled
      LockStatusUnlocked
      (SelfDeletingMessagesConfig 0)

instance IsFeatureConfig ClassifiedDomainsConfig where
  type FeatureSymbol ClassifiedDomainsConfig = "classifiedDomains"
  defFeatureStatus =
    WithStatus
      FeatureStatusDisabled
      LockStatusUnlocked
      (ClassifiedDomainsConfig [])

instance IsFeatureConfig AppLockConfig where
  type FeatureSymbol AppLockConfig = "appLock"
  defFeatureStatus =
    WithStatus
      FeatureStatusEnabled
      LockStatusUnlocked
      (AppLockConfig (EnforceAppLock False) 60)

instance IsFeatureConfig ConferenceCallingConfig where
  type FeatureSymbol ConferenceCallingConfig = "conferenceCalling"
  defFeatureStatus = WithStatus FeatureStatusEnabled LockStatusUnlocked ConferenceCallingConfig

instance IsFeatureConfig LegalHoldConfig where
  type FeatureSymbol LegalHoldConfig = "legalhold"
  defFeatureStatus = WithStatus FeatureStatusDisabled LockStatusUnlocked LegalHoldConfig

instance IsFeatureConfig SSOConfig where
  type FeatureSymbol SSOConfig = "sso"
  defFeatureStatus = WithStatus FeatureStatusDisabled LockStatusUnlocked SSOConfig

instance IsFeatureConfig SearchVisibilityConfig where
  type FeatureSymbol SearchVisibilityConfig = "searchVisibility"
  defFeatureStatus = WithStatus FeatureStatusDisabled LockStatusUnlocked SearchVisibilityConfig

instance IsFeatureConfig DigitalSignaturesConfig where
  type FeatureSymbol DigitalSignaturesConfig = "digitalSignatures"
  defFeatureStatus = WithStatus FeatureStatusDisabled LockStatusUnlocked DigitalSignaturesConfig

----------------------------------------------------------------------
-- TeamFeatureStatus

data IncludeLockStatus = WithLockStatus | WithoutLockStatus

-- type family TeamFeatureStatus (ps :: IncludeLockStatus) (a :: TeamFeatureName) :: * where
--   TeamFeatureStatus _ 'TeamFeatureLegalHold = TeamFeatureStatusNoConfig
--   TeamFeatureStatus _ 'TeamFeatureSSO = TeamFeatureStatusNoConfig
--   TeamFeatureStatus _ 'TeamFeatureSearchVisibility = TeamFeatureStatusNoConfig
--   TeamFeatureStatus _ 'TeamFeatureValidateSAMLEmails = TeamFeatureStatusNoConfig
--   TeamFeatureStatus _ 'TeamFeatureDigitalSignatures = TeamFeatureStatusNoConfig
--   TeamFeatureStatus _ 'TeamFeatureAppLock = TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
--   TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureFileSharing = TeamFeatureStatusNoConfig
--   TeamFeatureStatus 'WithLockStatus 'TeamFeatureFileSharing = TeamFeatureStatusNoConfigAndLockStatus
--   TeamFeatureStatus _ 'TeamFeatureClassifiedDomains = TeamFeatureStatusWithConfig TeamFeatureClassifiedDomainsConfig
--   TeamFeatureStatus _ 'TeamFeatureConferenceCalling = TeamFeatureStatusNoConfig
--   TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureSelfDeletingMessages = TeamFeatureStatusWithConfig SelfDeletingMessagesConfig
--   TeamFeatureStatus 'WithLockStatus 'TeamFeatureSelfDeletingMessages = TeamFeatureStatusWithConfigAndLockStatus SelfDeletingMessagesConfig
--   TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureGuestLinks = TeamFeatureStatusNoConfig
--   TeamFeatureStatus 'WithLockStatus 'TeamFeatureGuestLinks = TeamFeatureStatusNoConfigAndLockStatus
--   TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureSndFactorPasswordChallenge = TeamFeatureStatusNoConfig
--   TeamFeatureStatus 'WithLockStatus 'TeamFeatureSndFactorPasswordChallenge = TeamFeatureStatusNoConfigAndLockStatus
--   TeamFeatureStatus _ 'TeamFeatureSearchVisibilityInbound = TeamFeatureStatusNoConfig

-- type family FeatureHasNoConfig (ps :: IncludeLockStatus) (a :: TeamFeatureName) :: Constraint where
--   FeatureHasNoConfig 'WithLockStatus a = (TeamFeatureStatus 'WithLockStatus a ~ TeamFeatureStatusNoConfigAndLockStatus)
--   FeatureHasNoConfig 'WithoutLockStatus a = (TeamFeatureStatus 'WithoutLockStatus a ~ TeamFeatureStatusNoConfig)

-- if you add a new constructor here, don't forget to add it to the swagger (1.2) docs in "Wire.API.Swagger"!
-- modelForTeamFeature :: TeamFeatureName -> Doc.Model
-- modelForTeamFeature TeamFeatureLegalHold = modelTeamFeatureStatusNoConfig
-- modelForTeamFeature TeamFeatureSSO = modelTeamFeatureStatusNoConfig
-- modelForTeamFeature TeamFeatureSearchVisibility = modelTeamFeatureStatusNoConfig
-- modelForTeamFeature TeamFeatureValidateSAMLEmails = modelTeamFeatureStatusNoConfig
-- modelForTeamFeature TeamFeatureDigitalSignatures = modelTeamFeatureStatusNoConfig
-- modelForTeamFeature name@TeamFeatureAppLock = modelTeamFeatureStatusWithConfig name modelTeamFeatureAppLockConfig
-- modelForTeamFeature TeamFeatureFileSharing = modelTeamFeatureStatusNoConfig
-- modelForTeamFeature name@TeamFeatureClassifiedDomains = modelTeamFeatureStatusWithConfig name modelTeamFeatureClassifiedDomainsConfig
-- modelForTeamFeature TeamFeatureConferenceCalling = modelTeamFeatureStatusNoConfig
-- modelForTeamFeature name@TeamFeatureSelfDeletingMessages = modelTeamFeatureStatusWithConfig name modelSelfDeletingMessagesConfig
-- modelForTeamFeature TeamFeatureGuestLinks = modelTeamFeatureStatusNoConfig
-- modelForTeamFeature TeamFeatureSndFactorPasswordChallenge = modelTeamFeatureStatusNoConfig
-- modelForTeamFeature TeamFeatureSearchVisibilityInbound = modelTeamFeatureStatusNoConfig

data AllFeatureConfigs = AllFeatureConfigs
  { afcLegalholdStatus :: WithStatus LegalHoldConfig,
    afcSSOStatus :: WithStatus SSOConfig,
    afcTeamSearchVisibilityAvailable :: WithStatus SearchVisibilityConfig,
    afcValidateSAMLEmails :: WithStatus ValidateSAMLEmailsConfig,
    afcDigitalSignatures :: WithStatus DigitalSignaturesConfig,
    afcAppLock :: WithStatus AppLockConfig,
    afcFileSharing :: WithStatus FileSharingConfig,
    afcClassifiedDomains :: WithStatus ClassifiedDomainsConfig,
    afcConferenceCalling :: WithStatus ConferenceCallingConfig,
    afcSelfDeletingMessages :: WithStatus SelfDeletingMessagesConfig,
    afcGuestLink :: WithStatus GuestLinksConfig,
    afcSndFactorPasswordChallenge :: WithStatus SndFactorPasswordChallengeConfig
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AllFeatureConfigs)

instance ToSchema AllFeatureConfigs where
  schema =
    object "AllFeatureConfigs" $
      AllFeatureConfigs
        <$> afcLegalholdStatus .= featureField
        <*> afcSSOStatus .= featureField
        <*> afcTeamSearchVisibilityAvailable .= featureField
        <*> afcValidateSAMLEmails .= featureField
        <*> afcDigitalSignatures .= featureField
        <*> afcAppLock .= featureField
        <*> afcFileSharing .= featureField
        <*> afcClassifiedDomains .= featureField
        <*> afcConferenceCalling .= featureField
        <*> afcSelfDeletingMessages .= featureField
        <*> afcGuestLink .= featureField
        <*> afcSndFactorPasswordChallenge .= featureField
    where
      featureField ::
        forall cfg.
        (ToSchema cfg, KnownSymbol (FeatureSymbol cfg)) =>
        ObjectSchema SwaggerDoc (WithStatus cfg)
      featureField = field (T.pack (symbolVal (Proxy @(FeatureSymbol cfg)))) schema

instance Arbitrary AllFeatureConfigs where
  arbitrary =
    AllFeatureConfigs
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

----------------------------------------------------------------------
-- TeamFeatureStatusNoConfig

-- newtype TeamFeatureStatusNoConfig = TeamFeatureStatusNoConfig
--   { tfwoStatus :: FeatureStatus
--   }
--   deriving newtype (Eq, Show, Generic, Typeable, Arbitrary)
--   deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamFeatureStatusNoConfig)

-- modelTeamFeatureStatusNoConfig :: Doc.Model
-- modelTeamFeatureStatusNoConfig = Doc.defineModel "TeamFeatureStatusNoConfig" $ do
--   Doc.description "Team feature that has no configuration beyond the boolean on/off switch."
--   Doc.property "status" typeFeatureStatus $ Doc.description "status"

-- instance ToSchema TeamFeatureStatusNoConfig where
--   schema =
--     object "TeamFeatureStatusNoConfig" $
--       TeamFeatureStatusNoConfig
--         <$> tfwoStatus .= field "status" schema

-- data TeamFeatureStatusNoConfigAndLockStatus = TeamFeatureStatusNoConfigAndLockStatus
--   { tfwoapsStatus :: FeatureStatus,
--     tfwoapsLockStatus :: LockStatusValue
--   }
--   deriving stock (Eq, Show, Generic, Typeable)
--   deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamFeatureStatusNoConfigAndLockStatus)

-- instance Arbitrary TeamFeatureStatusNoConfigAndLockStatus where
--   arbitrary = TeamFeatureStatusNoConfigAndLockStatus <$> arbitrary <*> arbitrary

-- modelTeamFeatureStatusNoConfigAndLockStatus :: Doc.Model
-- modelTeamFeatureStatusNoConfigAndLockStatus = Doc.defineModel "TeamFeatureStatusNoConfigAndLockStatus" $ do
--   Doc.description "Team feature that has no configuration beyond the boolean on/off switch and a lock status"
--   Doc.property "status" typeFeatureStatus $ Doc.description ""
--   Doc.property "lockStatus" typeLockStatusValue $ Doc.description ""

-- instance ToSchema TeamFeatureStatusNoConfigAndLockStatus where
--   schema =
--     object "TeamFeatureStatusNoConfigAndLockStatus" $
--       TeamFeatureStatusNoConfigAndLockStatus
--         <$> tfwoapsStatus .= field "status" schema
--         <*> tfwoapsLockStatus .= field "lockStatus" schema

----------------------------------------------------------------------
-- WithStatus

-- | The support for disabled features with configs is intentional:
-- for instance, we want to be able to keep the config of a feature
-- that is turned on and off occasionally, and so not force the admin
-- to recreate the config every time it's turned on.
data WithStatus (cfg :: *) = WithStatus
  { wsStatus :: FeatureStatus,
    wsLockStatus :: LockStatus,
    wsConfig :: cfg
  }
  deriving stock (Eq, Show, Generic, Typeable, Functor)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (WithStatus cfg))

instance Arbitrary cfg => Arbitrary (WithStatus cfg) where
  arbitrary = WithStatus <$> arbitrary <*> arbitrary <*> arbitrary

-- modelTeamFeatureStatusWithConfig :: TeamFeatureName -> Doc.Model -> Doc.Model
-- modelTeamFeatureStatusWithConfig name cfgModel = Doc.defineModel (cs $ show name) $ do
--   Doc.description $ "Status and config of " <> cs (show name)
--   Doc.property "status" typeFeatureStatus $ Doc.description "status"
--   Doc.property "config" (Doc.ref cfgModel) $ Doc.description "config"

instance ToSchema cfg => ToSchema (WithStatus cfg) where
  schema =
    object name $
      WithStatus
        <$> wsStatus .= field "status" schema
        <*> wsLockStatus .= field "lockStatus" schema
        <*> wsConfig .= field "config" inner
    where
      inner = schema @cfg
      name = fromMaybe "" (getName (schemaDoc inner)) <> ".WithStatus"

--------------------------------------------------------------------------------
-- LegalHold feature

data LegalHoldConfig = LegalHoldConfig
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- SSO feature

data SSOConfig = SSOConfig
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- SearchVisibility feature

data SearchVisibilityConfig = SearchVisibilityConfig
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- ValidateSAMLEmails feature

data ValidateSAMLEmailsConfig = ValidateSAMLEmailsConfig
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- DigitalSignatures feature

data DigitalSignaturesConfig = DigitalSignaturesConfig
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- ConferenceCalling feature

data ConferenceCallingConfig = ConferenceCallingConfig
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- GuestLinks feature

data GuestLinksConfig = GuestLinksConfig
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- SndFactorPasswordChallenge feature

data SndFactorPasswordChallengeConfig = SndFactorPasswordChallengeConfig
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- SearchVisibilityInbound feature

data SearchVisibilityInboundConfig = SearchVisibilityInboundConfig
  deriving stock (Eq, Show)

----------------------------------------------------------------------
-- ClassifiedDomains feature

newtype ClassifiedDomainsConfig = ClassifiedDomainsConfig
  { classifiedDomainsDomains :: [Domain]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ClassifiedDomainsConfig)

deriving via (GenericUniform ClassifiedDomainsConfig) instance Arbitrary ClassifiedDomainsConfig

instance ToSchema ClassifiedDomainsConfig where
  schema =
    object "ClassifiedDomainsConfig" $
      ClassifiedDomainsConfig
        <$> classifiedDomainsDomains .= field "domains" (array schema)

modelTeamFeatureClassifiedDomainsConfig :: Doc.Model
modelTeamFeatureClassifiedDomainsConfig =
  Doc.defineModel "TeamFeatureClassifiedDomainsConfig" $ do
    Doc.property "domains" (Doc.array Doc.string') $ Doc.description "domains"

----------------------------------------------------------------------
-- AppLock feature

data AppLockConfig = AppLockConfig
  { applockEnforceAppLock :: EnforceAppLock,
    applockInactivityTimeoutSecs :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AppLockConfig)

deriving via (GenericUniform AppLockConfig) instance Arbitrary AppLockConfig

instance ToSchema AppLockConfig where
  schema =
    object "AppLockConfig" $
      AppLockConfig
        <$> applockEnforceAppLock .= field "enforceAppLock" schema
        <*> applockInactivityTimeoutSecs .= field "inactivityTimeoutSecs" schema

newtype EnforceAppLock = EnforceAppLock Bool
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON) via (Schema EnforceAppLock)

instance ToSchema EnforceAppLock where
  schema = EnforceAppLock <$> (\(EnforceAppLock v) -> v) .= schema

modelTeamFeatureAppLockConfig :: Doc.Model
modelTeamFeatureAppLockConfig =
  Doc.defineModel "TeamFeatureAppLockConfig" $ do
    Doc.property "enforceAppLock" Doc.bool' $ Doc.description "enforceAppLock"
    Doc.property "inactivityTimeoutSecs" Doc.int32' $ Doc.description ""

--------------------------------------------------------------------------------
-- FileSharing feature

data FileSharingConfig = FileSharingConfig
  deriving stock (Eq, Show)

----------------------------------------------------------------------
-- SelfDeletingMessagesConfig

newtype SelfDeletingMessagesConfig = SelfDeletingMessagesConfig
  { sdmEnforcedTimeoutSeconds :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema SelfDeletingMessagesConfig)
  deriving (Arbitrary) via (GenericUniform SelfDeletingMessagesConfig)

instance ToSchema SelfDeletingMessagesConfig where
  schema =
    object "SelfDeletingMessagesConfig" $
      SelfDeletingMessagesConfig
        <$> sdmEnforcedTimeoutSeconds .= field "enforcedTimeoutSeconds" schema

modelSelfDeletingMessagesConfig :: Doc.Model
modelSelfDeletingMessagesConfig =
  Doc.defineModel "SelfDeletingMessagesConfig" $ do
    Doc.property "enforcedTimeoutSeconds" Doc.int32' $ Doc.description "optional; default: `0` (no enforcement)"

----------------------------------------------------------------------
-- LockStatus

data LockStatus = LockStatusLocked | LockStatusUnlocked
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LockStatus)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema LockStatus)

instance FromHttpApiData LockStatus where
  parseUrlPiece = maybeToEither "Invalid lock status" . fromByteString . cs

typeLockStatusValue :: Doc.DataType
typeLockStatusValue =
  Doc.string $
    Doc.enum
      [ "locked",
        "unlocked"
      ]

instance ToSchema LockStatus where
  schema =
    enum @Text "LockStatus" $
      mconcat
        [ element "locked" LockStatusLocked,
          element "unlocked" LockStatusUnlocked
        ]

instance ToByteString LockStatus where
  builder LockStatusLocked = "locked"
  builder LockStatusUnlocked = "unlocked"

instance FromByteString LockStatus where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Right "locked" -> pure LockStatusLocked
        Right "unlocked" -> pure LockStatusUnlocked
        Right t -> fail $ "Invalid LockStatus: " <> T.unpack t
        Left e -> fail $ "Invalid LockStatus: " <> show e

instance Cass.Cql LockStatus where
  ctype = Cass.Tagged Cass.IntColumn

  fromCql (Cass.CqlInt n) = case n of
    0 -> pure LockStatusLocked
    1 -> pure LockStatusUnlocked
    _ -> Left "fromCql: Invalid LockStatus"
  fromCql _ = Left "fromCql: LockStatus: CqlInt expected"

  toCql LockStatusLocked = Cass.CqlInt 0
  toCql LockStatusUnlocked = Cass.CqlInt 1

----------------------------------------------------------------------
-- defaults

----------------------------------------------------------------------
-- internal

data LowerCaseFirst

instance StringModifier LowerCaseFirst where
  getStringModifier (x : xs) = toLower x : xs
  getStringModifier [] = []
