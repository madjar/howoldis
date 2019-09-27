{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Channels (Channel (..), label, humantime, jobset, commit, channels, parseVersion) where

import Control.Concurrent.ParallelIO.Global (parallelE)
import Control.Exception (throwIO, try)
import Control.Lens ((^.))
import Control.Monad (unless)
import GHC.Generics
import qualified Haquery as HQ
import Data.Aeson (ToJSON)
import Data.Either (rights, lefts)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.List (sortOn, null)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))
import Data.Time.Clock (UTCTime(..), NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Zones
import Data.Time.Zones.All
import qualified Data.Text as DT
import qualified Data.ByteString.Char8 as C8
import Data.Text (pack, unpack, replace, Text)
import qualified Network.Wreq as W
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as WS
import Network.HTTP.Client (HttpException(HttpExceptionRequest)
                           , HttpExceptionContent(StatusCodeException)
                           , responseHeaders)


data RawChannel = RawChannel { rname :: Text
                             , rtime :: UTCTime
                             } deriving (Show)

data Channel = Channel { name  :: Text
                       , time :: NominalDiffTime
                       , link :: String
                       } deriving (Show, Generic)
instance ToJSON Channel

parseVersion :: Channel -> Text
parseVersion = DT.takeWhile (/= '-') . DT.drop 1 . DT.dropWhile (/= '-') . name

parseTime :: String -> UTCTime
parseTime = fromMaybe nixEpoch . fmap (localTimeToUTCTZ tz) . parseTimeM @Maybe True defaultTimeLocale "%F %R"
  where tz = tzByLabel Europe__Rome -- CET/CEST
        nixEpoch = UTCTime (fromGregorian 2006 1 1) 0

findGoodChannels :: Text -> [RawChannel]
findGoodChannels html = map rowToChannel rows
  where
    rows = drop 2 $ concatMap (HQ.select "tr:has(a)") $ HQ.parseHtml html
    rowToChannel tag = RawChannel {..}
      where rname = replaceEscapedQuotes $ fromMaybe "" $ HQ.attr "href" $ head $ HQ.select "a" tag
            rtime = parseTime $ unpack $ innerText $ head $ HQ.select "*:nth-child(3)" tag
            -- TODO: Why do these quotes leak into tag content?
            replaceEscapedQuotes = replace "\\\"" ""

-- TODO: Remove once merged https://github.com/crufter/haquery/pull/6
innerText :: HQ.Tag -> Text
innerText (HQ.Doctype _ text) = text
innerText (HQ.Text _ text) = text
innerText (HQ.Tag _ _ _ children) = DT.concat $ map innerText children

makeChannel :: Session -> UTCTime -> RawChannel -> IO Channel
makeChannel sess current channel = do
  res <- try $ WS.head_ sess . unpack $ "https://nixos.org/channels/" <> rname channel
  link <- case res of
               -- We get 302 redirect with Location header, no need to go further
               -- Propagate the rest of the errors
               Left (HttpExceptionRequest _ (StatusCodeException resp _)) ->
                 return $ C8.unpack $ snd $ head $ filter ((== "Location") . fst) . responseHeaders $ resp
               Right response -> return $ C8.unpack $ response ^. W.responseHeader "Location"
               Left e -> throwIO e
  let diff = diffUTCTime current (rtime channel)
  return $ Channel (rname channel)
                    (diff)
                    link

-- |The list of the current NixOS channels
channels :: IO [Channel]
channels = do
    sess <- WS.newAPISession
    r <- WS.get sess "https://nixos.org/channels/"
    current <- getCurrentTime
    let html = pack $ show $ r ^. W.responseBody
    responseOrExc <- parallelE $
      makeChannel sess current <$> findGoodChannels html
    unless (null $ lefts responseOrExc) $ print $ lefts responseOrExc
    return $ sortOn (Down . parseVersion) $ rights responseOrExc



-- Display helpers


data Label = Danger | Warning | Success | NoLabel deriving (Generic)
instance Show Label where
  show Danger = "danger"
  show Warning = "warning"
  show Success = "success"
  show NoLabel = ""
instance ToJSON Label


label :: Channel -> Label
label = diffToLabel . time


-- | Takes time since last update to the channel and colors it based on it's age
diffToLabel :: NominalDiffTime -> Label
diffToLabel time
  | days < 3 = Success
  | days < 10 = Warning
  | otherwise = Danger
    where days = time / (60 * 60 * 24)


humantime :: Channel -> Text
humantime = humanTimeDiff . time


humanTimeDiff :: NominalDiffTime -> Text
humanTimeDiff d
  | days > 1 = doShow days (pluralize days "day" "days")
  | hours > 1 = doShow hours (pluralize hours "hour" "hours")
  | minutes > 1 = doShow minutes (pluralize minutes "minute" "minutes")
  | otherwise = doShow d (pluralize d "second" "seconds")
  where minutes = d / 60
        hours = minutes / 60
        days = hours / 24
        doShow x unit = pack (show @Int $ truncate x) <> " " <> unit

        -- diffUTCTime contains a float-like value, so everything below `2` will be treated as 1
        -- Furthermore a 0 causes the next lower unit, so only 2 and more will be interpreted
        -- as plural.
        pluralize n s p
          | n < 2 = s
          | otherwise = p

jobset :: Channel -> Maybe Text
jobset = toJobset . name

toJobset :: Text -> Maybe Text
toJobset c
 | c == "nixos-unstable"       = Just "nixos/trunk-combined/tested"
 | c == "nixos-unstable-small" = Just "nixos/unstable-small/tested"
 | c == "nixpkgs-unstable"     = Just "nixpkgs/trunk/unstable"
 | "nixos-" `DT.isPrefixOf` c  = Just $ "nixos/release-" <> DT.drop 6 c <> "/tested"
 | "-darwin" `DT.isSuffixOf` c = Just $ "nixpkgs/" <> c <> "/darwin-tested"
 | otherwise                   = Nothing


commit :: Channel -> String
commit = parseCommit . link

parseCommit :: String -> String
parseCommit url = last $ splitOn "." url
