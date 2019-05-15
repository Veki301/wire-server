module API.Teams.LegalHold (tests) where

import Imports
import API.Util hiding (createTeam)
-- import Bilge hiding (timeout)
-- import Bilge.Assert
-- import Control.Lens hiding ((#), (.=))
-- import Data.Aeson hiding (json)
-- import Data.Aeson.Lens
-- import Data.ByteString.Conversion
import Data.Id
-- import Data.List1
-- import Data.Misc (PlainTextPassword (..))
-- import Data.Range
-- import Galley.Types hiding (EventType (..), EventData (..), MemberUpdate (..))
-- import Galley.Types.Teams
-- import Galley.Types.Teams.Intra
-- import Gundeck.Types.Notification
import Test.Tasty
import Test.Tasty.HUnit (assertBool)
import TestSetup
import API.SQS
import Brig.Types.Client

import qualified API.Util as Util
-- import qualified Data.Currency as Currency
-- import qualified Data.List1 as List1
-- import qualified Data.Set as Set
-- import qualified Data.Text as T
-- import qualified Data.UUID as UUID
-- import qualified Galley.Types as Conv
-- import qualified Network.Wai.Utilities.Error as Error


tests :: IO TestSetup -> TestTree
tests s = testGroup "Teams LegalHold API"
    [ -- device handling (CRUD)
      test s "POST /teams/{tid}/legalhold/{uid}" testCreateLegalHoldDevice
    , test s "POST /teams/{tid}/legalhold/{uid} - twice" testCreateTwoLegalHoldDevices
    , test s "PUT /teams/{tid}/legalhold/{uid}/approve" testApproveLegalHoldDevice
    , test s "(user denies approval: nothing needs to be done in backend)" (pure ())
    , test s "GET /teams/{tid}/legalhold/{uid}" testGetLegalHoldDeviceStatus
    , test s "DELETE /teams/{tid}/legalhold/{uid}" testRemoveLegalHoldDevice

      -- legal hold settings
    , test s "POST /teams/{tid}/legalhold/settings" testCreateLegalHoldTeamSettings
    , test s "GET /teams/{tid}/legalhold/settings" testGetLegalHoldTeamSettings
    , test s "DELETE /teams/{tid}/legalhold/settings" testRemoveLegalHoldFromTeam
    , test s "GET, PUT /i/teams/{tid}/legalhold?enabled={true,false}" testEnablePerTeam

      -- behavior of existing end-points
    , test s "POST /clients" testCreateLegalHoldDeviceOldAPI
    , test s "DELETE /clients/{cid}" testDeleteLegalHoldDeviceOldAPI

{- TODO:
    zauth/libzauth level: Allow access to legal hold service tokens
        conversations/{cnv}/otr/messages
        /notifications
        /access
        (maybe others?)
    conversations/{cnv}/otr/messages - possibly show the legal hold device (if missing) as a different device type (or show that on device level, depending on how client teams prefer)
    GET /team/{tid}/members - show legal hold status of all members

  TODO: feature flag!  (not sure tests are needed for this.)

-}

    ]


-- TODO: delete this function when we're done fixing all the test cases.
ignore :: Monad m => a -> m ()
ignore a = traceShow "\n*** ignored test case!!\n" $ pure ()


testCreateLegalHoldDevice :: TestM ()
testCreateLegalHoldDevice = do
    pure ()

    -- team admin, owner can do it.  (implemented via new internal permission bit.)
    -- member can't do it.
    -- fail if legal hold service is disabled for team
    -- fail if legal hold service is disabled via feature flag
    -- all of user's clients receive an event
    -- contacts team's legal hold service and establishes a cryptobox
    -- responds with public key etc of legal hold device
    -- requests approval from monitored user asynchronously; request contains pre-keys


testCreateTwoLegalHoldDevices :: TestM ()
testCreateTwoLegalHoldDevices = do
    pure ()

    -- creating a second valid a legal hold device is rejected.
    -- also when the legal hold status of a user is 'pending".


testApproveLegalHoldDevice :: TestM ()
testApproveLegalHoldDevice = do
    pure ()

    -- only user themself can do it
    -- fail if no legal hold service registered
    -- fail if legal Hold feature flag disabled
    -- generates and stores legalhold tokens/cookies
    -- synchronously sends tokens/cookies to team's legal hold service
    -- expect return payload from service to contain device creation information (NewClient)
    -- adds device to user's list of devices
    -- sends an event to all user's clients
    -- sends an event to team settings (however that works; it's a client-independent event i think)
    -- all of user's communication peers receive an event


testGetLegalHoldDeviceStatus :: TestM ()
testGetLegalHoldDeviceStatus = do
    pure ()

    -- Show whether enabled, pending, disabled


testRemoveLegalHoldDevice :: TestM ()
testRemoveLegalHoldDevice = do
    pure ()

    -- Remove Legal hold device from user.
    -- send event to all of user's devices
    -- send event to all communication peers
    -- when still pending, notify clients they no longer need approval if deleted when still pending



-- | This type is analogous to 'NewService' for bots.
data NewLegalHoldService = NewLegalHoldService
    { newLegalHoldServiceUrl     :: !HttpsUrl
    , newLegalHoldServiceKey     :: !ServiceKeyPEM
    , newLegalHoldServiceToken   :: !(Maybe ServiceToken)
    }

instance ToJSON NewLegalHoldService where
    toJSON = undefined

instance FromJSON NewLegalHoldService where
    fromJSON = undefined



testCreateLegalHoldTeamSettings :: TestM ()
testCreateLegalHoldTeamSettings = do
    (owner, tid) <- createTeam

    -- bad key
    let Just newUrl = fromByteString "https://new.localhost/"
        Right [k] = pemParseBS "-----BEGIN PUBLIC KEY-----\n\n-----END PUBLIC KEY-----"
        newBad :: _ = new { newServiceKey = ServiceKeyPEM k }
        newService = NewLegalHoldService newUrl newBad Nothing
    postSettings newService !!! const 400 === statusCode

    -- ...
    let Just newUrl = fromByteString "https://new.localhost/"
        newService = NewLegalHoldService newUrl _ Nothing
    postSettings newService !!! const 400 === statusCode



--     POST /teams/{tid}/legalhold/settings

    pure ()

    -- only allowed for users with corresp. permission bit
    -- only allowed if feature is on globally
    -- only allowed if team has feature bit set
    -- checks /status of legal hold service
    -- /status returns 5xx if unavailable
    -- legal hold device identity is authentic: the public key in the create request payload needs to match a signature in the /status response.
    -- after this, corresp. entry in cassandra will exist in a table with index TeamId.  (the entry must contain the public key from the request.)



testAddGetServiceBadKey :: Config -> DB.ClientState -> Brig -> Http ()
testAddGetServiceBadKey config db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    -- Add service
    new <- defNewService config
    -- Specially crafted key that passes basic validation
    let Right [k] = pemParseBS "-----BEGIN PUBLIC KEY-----\n\n-----END PUBLIC KEY-----"
    let newBad = new { newServiceKey = ServiceKeyPEM k }
    addService brig pid newBad !!! const 400 === statusCode




testGetLegalHoldTeamSettings :: TestM ()
testGetLegalHoldTeamSettings = do
    pure ()

    -- only allowed for users with corresp. permission bit
    -- only allowed if feature is on globally
    -- only allowed if team has feature bit set
    -- returns 404 if team is not under legal hold
    -- returns table entry if team is under legal hold


testRemoveLegalHoldFromTeam :: TestM ()
testRemoveLegalHoldFromTeam = do
    pure ()

    -- only allowed for users with corresp. permission bit
    -- only allowed if feature is on globally
    -- only allowed if team has feature bit set
    -- after this, corresp. entry in cassandra will *NOT* exist


testEnablePerTeam :: TestM ()
testEnablePerTeam = do
    pure ()

    -- TODO: ...


testCreateLegalHoldDeviceOldAPI :: TestM ()
testCreateLegalHoldDeviceOldAPI = do
    -- regular users cannot create LegalHoldClients
    let lk = (someLastPrekeys !! 0)
    u <- randomUser

    -- TODO: requests to /clients with type=LegalHoldClientType should fail  (400 instead of 201)
    void $ randomClientWithType LegalHoldClientType 201 u lk

    -- team users cannot create LegalHoldClients
    (owner, _) <- createTeam

    -- TODO: requests to /clients with type=LegalHoldClientType should fail (400 instead of 201)
    void $ randomClientWithType LegalHoldClientType 201 owner lk
    exactlyOneLegalHoldDevice owner

    -- TODO: the remainder of this test can be removed once `POST /clients` does not work any
    -- more for legal hold devices.
    void $ randomClientWithType LegalHoldClientType 201 owner lk  -- overwrite
    exactlyOneLegalHoldDevice owner


testDeleteLegalHoldDeviceOldAPI :: TestM ()
testDeleteLegalHoldDeviceOldAPI = do
    pure ()

    -- legal hold device cannot be deleted by anybody, ever.


----------------------------------------------------------------------
-- helpers

createTeam :: TestM (UserId, TeamId)
createTeam = do
    ownerid <- Util.randomUser
    teamid <- Util.createTeamInternal "foo" ownerid
    assertQueue "create team" tActivate
    pure (ownerid, teamid)

exactlyOneLegalHoldDevice :: UserId -> TestM ()
exactlyOneLegalHoldDevice uid = do
    clients :: [Client]
        <- getClients uid >>= maybe (error $ "decodeBody: [Client]") pure . decodeBody
    liftIO $ do
        let numdevs = length $ clientType <$> clients
        assertBool ("no legal hold device for user " <> show uid) (numdevs > 0)
        assertBool ("more than one legal hold device for user " <> show uid) (numdevs < 2)
