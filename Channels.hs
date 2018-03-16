{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Channels (Channel (..), channels) where

import Control.Concurrent.ParallelIO.Global (parallelE, stopGlobalPool)
import Control.Exception (try)
import Control.Lens
import Control.Monad (unless)
import GHC.Generics
import qualified Haquery as HQ
import Data.Aeson (ToJSON)
import Data.Either (rights, lefts)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.List (null, sortBy)
import Data.List.Split (splitOn)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.LocalTime (localTimeToUTC, hoursToTimeZone)
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
                             , rtime :: Either Text UTCTime
                             } deriving (Show)

data Channel = Channel { name  :: Text
                       , label :: Label
                       , humantime  :: Text
                       , time :: Maybe NominalDiffTime
                       , commit :: String
                       , link :: String
                       , jobset :: Maybe Text
                       } deriving (Show, Generic)
instance ToJSON Channel

instance Eq Channel where
  s == c = name s == name c

instance Ord Channel where
  s `compare` c = name s `compare` name c

data Label = Danger | Warning | Success | NoLabel deriving (Generic)
instance Show Label where
  show Danger = "danger"
  show Warning = "warning"
  show Success = "success"
  show NoLabel = ""
instance ToJSON Label

parseTime :: String -> Either Text UTCTime
parseTime = fmap (localTimeToUTCTZ tz) . parseTimeM True defaultTimeLocale "%F %R"
  where tz = tzByLabel Europe__Rome -- CET/CEST

findGoodChannels :: Text -> [RawChannel]
findGoodChannels html = map makeChannel rows
  where
    rows = drop 2 $ concatMap (HQ.select "tr:has(a)") $ HQ.parseHtml html
    makeChannel tag = RawChannel name time
      where name = replaceEscapedQuotes $ fromMaybe "" $ HQ.attr "href" $ head $ HQ.select "a" tag
            time = parseTime $ unpack $ innerText $ head $ HQ.select "*:nth-child(3)" tag
            -- TODO: Why do these quotes leak into tag content?
            replaceEscapedQuotes = replace "\\\"" ""

-- TODO: Remove once merged https://github.com/crufter/haquery/pull/6
innerText :: HQ.Tag -> Text
innerText (HQ.Doctype _ text) = text
innerText (HQ.Text _ text) = text
innerText (HQ.Tag _ _ _ children) = DT.concat $ map innerText children

makeChannel :: Session -> UTCTime -> RawChannel -> IO Channel
makeChannel sess current channel = do
  e <- try $ WS.head_ sess . unpack $ "https://nixos.org/channels/" <> rname channel
  let link = case e of
               -- We get 302 redirect with Location header, no need to go further
               -- Propagate the rest of the errors
               Left (HttpExceptionRequest _ (StatusCodeException resp _)) ->
                 C8.unpack $ snd $ head $ filter ((== "Location") . fst) . responseHeaders $ resp
               Right response -> C8.unpack $ response ^. W.responseHeader "Location"
  let diff = diffUTCTime current <$> rtime channel
  return $ Channel (rname channel)
                    (diffToLabel diff)
                    (either id humanTimeDiff diff)
                    (either (const Nothing) Just diff)
                    (parseCommit link)
                    link
                    (toJobset $ rname channel)


parseCommit :: String -> String
parseCommit url = last $ splitOn "." url

-- |The list of the current NixOS channels
channels :: IO [Channel]
channels =
  WS.withAPISession $ \sess -> do
    r <- WS.get sess "https://nixos.org/channels/"
    current <- getCurrentTime
    let html = pack $ show $ r ^. W.responseBody
    responseOrExc <- parallelE $
      makeChannel sess current <$> findGoodChannels html
    unless (null $ lefts responseOrExc) $ print $ lefts responseOrExc
    return $ sortBy (flip compare) $ rights responseOrExc


humanTimeDiff :: NominalDiffTime -> Text
humanTimeDiff d
  | days > 1 = doShow days (pluralize days "day" "days")
  | hours > 1 = doShow hours (pluralize hours "hour" "hours")
  | minutes > 1 = doShow minutes (pluralize minutes "minute" "minutes")
  | otherwise = doShow d (pluralize d "second" "seconds")
  where minutes = d / 60
        hours = minutes / 60
        days = hours / 24
        doShow x unit = pack (show $ truncate x) <> " " <> unit

        -- diffUTCTime contains a float-like value, so everything below `2` will be treated as 1
        -- Furthermore a 0 causes the next lower unit, so only 2 and more will be interpreted
        -- as plural.
        pluralize d s p
          | d < 2 = s
          | otherwise = p

toJobset :: Text -> Maybe Text
toJobset c
 | c == "nixos-unstable"       = Just "nixos/trunk-combined/tested"
 | c == "nixos-unstable-small" = Just "nixos/unstable-small/tested"
 | c == "nixpkgs-unstable"     = Just "nixpkgs/trunk/unstable"
 | c == "nixpkgs-17.09-darwin" = Just "nixpkgs/nixpkgs-17.09-darwin/darwin-tested"
 | c == "nixpkgs-18.03-darwin" = Just "nixpkgs/nixpkgs-18.03-darwin/darwin-tested"
 | "nixos-" `DT.isPrefixOf` c  = Just $ "nixos/release-" <> DT.drop 6 c <> "/tested"
 | otherwise                   = Nothing

-- | Takes time since last update to the channel and colors it based on it's age
diffToLabel :: Either Text NominalDiffTime -> Label
diffToLabel (Left _) = NoLabel
diffToLabel (Right time)
  | days < 3 = Success
  | days < 10 = Warning
  | otherwise = Danger
    where days = time / (60 * 60 * 24)
