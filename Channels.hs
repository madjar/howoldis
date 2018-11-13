{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Channels
  ( Channel(..)
  , newEnv
  , channels
  ) where

import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Except (ExceptT(..), runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (MonadLogger, logInfoN)
import           Control.Monad.Reader (MonadReader, asks)
import           Data.Aeson (FromJSON(..), ToJSON, withObject, (.:))
import           Data.Bifunctor (first, second)
import qualified Data.ByteString.Char8 as CharBytes
import qualified Data.ByteString.Lazy as LazyBytes
import           Data.List (sortBy)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import           GHC.Generics
import           GitHub.Auth (Auth(..))
import           GitHub.Data (Owner)
import qualified GitHub.Data.GitData as GitHub
import           GitHub.Data.Name (Name)
import           GitHub.Data.Repos (Repo)
import           GitHub.Data.Request (Request, RW(..), query, toPathPart)
import qualified GitHub.Endpoints.GitData.Commits as GitHub
import           GitHub.Endpoints.Repos.Commits (Commit)
import           GitHub.Request (executeRequestWithMgr, executeRequestWithMgr')
import qualified Haquery as HQ
import           Network.HTTP.Client
  ( Manager
  , httpLbs
  , newManager
  , parseRequest
  , responseBody
  )
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Environment (lookupEnv)
import           UnliftIO (MonadUnliftIO)
import           UnliftIO.Async (mapConcurrently)

data HowoldisEnv = HowoldisEnv
  { howoldisEnvManager :: Manager
  , howoldisGitHubAuth :: Maybe Auth
  }

newEnv :: MonadIO m => m HowoldisEnv
newEnv = do
  mgr <- liftIO . newManager $ tlsManagerSettings
  auth <-
    fmap (OAuth . CharBytes.pack) <$> (liftIO . lookupEnv $ "GITHUB_TOKEN")
  return HowoldisEnv {howoldisEnvManager = mgr, howoldisGitHubAuth = auth}

github ::
     (MonadReader HowoldisEnv m, MonadIO m)
  => GitHub.Request 'RO b
  -> ExceptT GitHub.Error m b
github request = do
  mgr <- asks howoldisEnvManager
  mAuth <- asks howoldisGitHubAuth
  case mAuth of
    Just auth -> ExceptT . liftIO . executeRequestWithMgr mgr auth $ request
    Nothing -> ExceptT . liftIO . executeRequestWithMgr' mgr $ request

{-# ANN
  Branch ("HLint: ignore Use newtype instead of data" :: Text) #-}
data Branch = Branch
  { branchCommit :: Commit
  }

instance FromJSON Branch where
  parseJSON = withObject "Branch" $ \o -> Branch <$> o .: "commit"

branchR :: Name Owner -> Name Repo -> Name Branch -> Request k Branch
branchR user repo branch =
  query
    ["repos", toPathPart user, toPathPart repo, "branches", toPathPart branch]
    []

data Channel = Channel
  { name :: Text
  , label :: Label
  , humantime :: Text
  , time :: UTCTime
  , commit :: Text
  , link :: Text
  , jobset :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON Channel

parseVersion :: Text -> Text
parseVersion = matchVersion . Text.splitOn "-"

matchVersion :: [Text] -> Text
matchVersion (_:x:_) = x
matchVersion _ = ""

instance Eq Channel where
  s == c = (parseVersion . name $ s) == (parseVersion . name $ c)

instance Ord Channel where
  s `compare` c = (parseVersion . name $ s) `compare` (parseVersion . name $ c)

data Label
  = Danger
  | Warning
  | Success
  | NoLabel
  deriving (Generic)

instance Show Label where
  show Danger = "danger"
  show Warning = "warning"
  show Success = "success"
  show NoLabel = ""

instance ToJSON Label

getChannelNames :: Text -> [Text]
getChannelNames html = map makeChannelName rows
  where
    rows = drop 2 $ concatMap (HQ.select "tr:has(a)") $ HQ.parseHtml html
    makeChannelName =
      replaceEscapedQuotes .
      fromMaybe "" . HQ.attr "href" . head . HQ.select "a"
    replaceEscapedQuotes = Text.replace "\\\"" ""

devBranchName :: Text -> Text
devBranchName channel
  | isUnstable channel = "master"
  | otherwise =
    let version = parseVersion channel
     in "release-" <> version

isUnstable :: Text -> Bool
isUnstable channel = "unstable" `Text.isInfixOf` channel

releasedBranchName :: Text -> Text
releasedBranchName channel = channel

makeChannel ::
     (MonadLogger m, MonadIO m, MonadReader HowoldisEnv m)
  => Text
  -> m (Either GitHub.Error Channel)
makeChannel channel =
  runExceptT $ do
    dev <- latestCommitFor "nixpkgs" (devBranchName channel)
    released <- latestCommitFor "nixpkgs-channels" (releasedBranchName channel)
    let commitTime =
          GitHub.gitUserDate .
          GitHub.gitCommitCommitter . GitHub.commitGitCommit
        devTime = commitTime dev
        releasedTime = commitTime released
        diff = diffUTCTime devTime releasedTime
        ch =
          Channel
            { name = channel
            , label = diffToLabel diff
            , humantime = humanTimeDiff diff
            , time = releasedTime
            , commit =
                Text.take 11 . GitHub.untagName . GitHub.commitSha $ released
            , link = "https://nixos.org/channels/" <> channel
            , jobset = toJobset channel
            }
    ExceptT . return . Right $ ch

latestCommitFor ::
     (MonadLogger m, MonadIO m, MonadReader HowoldisEnv m)
  => Text
  -> Text
  -> ExceptT GitHub.Error m Commit
latestCommitFor repo branch = do
  logInfoN $ "Fetching " <> branch <> " of " <> repo
  fmap branchCommit .
    github . branchR "NixOS" (GitHub.mkName (Proxy :: Proxy Repo) repo) $
    GitHub.mkName (Proxy :: Proxy Branch) branch

-- | The list of the current NixOS channels
channels ::
     (MonadLogger m, MonadReader HowoldisEnv m, MonadUnliftIO m, MonadThrow m)
  => m (Either Text [Channel])
channels = do
  rq <- parseRequest "https://nixos.org/channels"
  mgr <- asks howoldisEnvManager
  r <- liftIO . httpLbs rq $ mgr
  let html = Text.decodeUtf8 . LazyBytes.toStrict . responseBody $ r
  responseOrExc <- mapConcurrently makeChannel . getChannelNames $ html
  return . first (Text.pack . show) . second (sortBy (flip compare)) . sequenceA $
    responseOrExc

humanTimeDiff :: NominalDiffTime -> Text
humanTimeDiff d
  | days > 1 = doShow days (pluralize days "day" "days")
  | hours > 1 = doShow hours (pluralize hours "hour" "hours")
  | minutes > 1 = doShow minutes (pluralize minutes "minute" "minutes")
  | otherwise = doShow d (pluralize d "second" "seconds")
  where
    minutes = d / 60
    hours = minutes / 60
    days = hours / 24
    doShow x unit = Text.pack (show @ Integer $ truncate x) <> " " <> unit
        -- diffUTCTime contains a float-like value, so everything below `2` will be treated as 1
        -- Furthermore a 0 causes the next lower unit, so only 2 and more will be interpreted
        -- as plural.
    pluralize d' s p
      | d' < 2 = s
      | otherwise = p

toJobset :: Text -> Maybe Text
toJobset c
  | c == "nixos-unstable" = Just "nixos/trunk-combined/tested"
  | c == "nixos-unstable-small" = Just "nixos/unstable-small/tested"
  | c == "nixpkgs-unstable" = Just "nixpkgs/trunk/unstable"
  | "nixos-" `Text.isPrefixOf` c =
    Just $ "nixos/release-" <> Text.drop 6 c <> "/tested"
  | "-darwin" `Text.isSuffixOf` c = Just $ "nixpkgs/" <> c <> "/darwin-tested"
  | otherwise = Nothing

-- | Takes time since last update to the channel and colors it based on it's age
diffToLabel :: NominalDiffTime -> Label
diffToLabel t
  | days < 3 = Success
  | days < 10 = Warning
  | otherwise = Danger
  where
    days = t / (60 * 60 * 24)

