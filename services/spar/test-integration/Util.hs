{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | TODO: this is all copied from /services/galley/test/integration/API/Util.hs and some other
-- places; should we make this a new library?
module Util where

import Bilge
import Bilge.Assert
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson as Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Id
import Data.Maybe
import Data.Range
import Data.String.Conversions
import Data.UUID as UUID hiding (null, fromByteString)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Lens.Micro
import Spar.API ()
import Test.Hspec
import Util.Options

import qualified Brig.Types.Common as Brig
import qualified Brig.Types.User as Brig
import qualified Galley.Types.Teams as Galley


data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { brig      :: Endpoint
  , galley    :: Endpoint
  , spar      :: Endpoint
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

type Brig = Request -> Request
type Galley = Request -> Request


-- from brig integration tests (the thing we actually need)

createUserWithTeam :: (MonadHttp m, MonadIO m) => Brig -> Galley -> m (UserId, TeamId)
createUserWithTeam brg gly = do
    e <- randomEmail
    n <- pure ("randomName" :: String)  -- TODO!
    let p = RequestBodyLBS . encode $ object
            [ "name"            .= n
            , "email"           .= Brig.fromEmail e
            , "password"        .= ("secret" :: String)
            , "team"            .= newTeam
            ]
    bdy <- decodeBody <$> post (brg . path "/i/users" . contentJson . body p)
    let (Just uid, Just (Just tid)) = (Brig.userId <$> bdy, Brig.userTeam <$> bdy)
    (team:_) <- (^. Galley.teamListTeams) <$> getTeams uid gly
    () <- Control.Exception.assert {- "Team ID in registration and team table do not match" -} (tid ==  team ^. Galley.teamId)
          $ pure ()
    selfTeam <- Brig.userTeam . Brig.selfUser <$> getSelfProfile brg uid
    () <- Control.Exception.assert {- "Team ID in self profile and team table do not match" -} (selfTeam == Just tid)
          $ pure ()
    return (uid, tid)

decodeBody :: FromJSON a => Response (Maybe LBS) -> Maybe a
decodeBody = responseBody >=> decode'

getTeams :: (MonadHttp m, MonadIO m) => UserId -> Galley -> m Galley.TeamList
getTeams u gly = do
    r <- get ( gly
             . paths ["teams"]
             . zAuthAccess u "conn"
             . expect2xx
             )
    return $ fromMaybe (error "getTeams: failed to parse response") (decodeBody r)

getSelfProfile :: (MonadHttp m, MonadIO m) => Brig -> UserId -> m Brig.SelfProfile
getSelfProfile brg usr = do
    rsp <- get $ brg . path "/self" . zUser usr
    return $ fromMaybe (error $ "getSelfProfile: failed to decode: " ++ show rsp) (decodeBody rsp)

zAuthAccess :: UserId -> SBS -> Request -> Request
zAuthAccess u c = header "Z-Type" "access" . zUser u . zConn c

newTeam :: Galley.BindingNewTeam
newTeam = Galley.BindingNewTeam $ Galley.newNewTeam (unsafeRange "teamName") (unsafeRange "defaultIcon")


-- from galley integration tests mostly (the thing we don't need)

randomUser :: (HasCallStack, MonadCatch m, MonadHttp m, MonadIO m) => Brig -> m UserId
randomUser b = do
    e <- liftIO randomEmail
    let p = object [ "name" .= Brig.fromEmail e, "email" .= Brig.fromEmail e, "password" .= ("secret" :: ST) ]
    r <- post (b . path "/i/users" . json p) <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

createTeam :: (HasCallStack, MonadCatch m, MonadHttp m, MonadIO m) => Galley -> ST -> UserId -> m TeamId
createTeam g name owner = do
    let mems :: [Galley.TeamMember] = []
    let mm = if null mems then Nothing else Just $ unsafeRange (take 127 mems)
    let nt_ = Galley.NonBindingNewTeam $ Galley.newNewTeam (unsafeRange name) (unsafeRange "icon") & Galley.newTeamMembers .~ mm
    resp <- post (g . path "/teams" . zUser owner . zConn "conn" . zType "access" . json nt_) <!! do
        const 201  === statusCode
        const True === isJust . getHeader "Location"
    fromBS (getHeader' "Location" resp)

randomEmail :: MonadIO m => m Brig.Email
randomEmail = do
    uid <- liftIO nextRandom
    return $ Brig.Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

fromBS :: (HasCallStack, FromByteString a, Monad m) => SBS -> m a
fromBS = maybe (fail "fromBS: no parse") return . fromByteString

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zConn :: SBS -> Request -> Request
zConn = header "Z-Connection"

zType :: SBS -> Request -> Request
zType = header "Z-Type"
