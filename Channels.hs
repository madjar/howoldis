{-# LANGUAGE OverloadedStrings #-}
module Channels (DiffChannel (..), channels, jobset) where

import Control.Concurrent.ParallelIO.Global (parallelE, stopGlobalPool)
import Control.Exception (try)
import Control.Lens
import Control.Monad (unless)
import qualified Haquery as HQ
import Data.Either (rights, lefts)
import Data.Monoid ((<>))
import Data.List (null)
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
import Network.HTTP.Client (HttpException(StatusCodeException))


data Channel = Channel { name :: Text
                       , time :: Either Text UTCTime
                       } deriving (Show)

data DiffChannel = DiffChannel { dname  :: Text
                               , dlabel :: Color
                               , dhumantime  :: Text
                               , dtime :: Maybe NominalDiffTime
                               , dcommit :: String
                               , dlink :: String
                               , djobset :: Maybe Text
                               } deriving (Show)

type Color = Text


parseTime :: String -> Either Text UTCTime
parseTime = fmap (localTimeToUTCTZ tz) . parseTimeM True defaultTimeLocale "%F %R"
  where tz = tzByLabel Europe__Rome -- CET/CEST

findGoodChannels :: Text -> [Channel]
findGoodChannels html = map makeChannel rows
  where
    rows = drop 2 $ concatMap (HQ.select "tr:has(a)") $ HQ.parseHtml $ html
    makeChannel tag = Channel name time
      where name = replaceEscapedQuotes $ maybe "" id $ HQ.attr "href" $ head $ HQ.select "a" tag
            time = parseTime $ unpack $ innerText $ head $ HQ.select "*:nth-child(3)" tag
            -- TODO: Why do these quotes leak into tag content?
            replaceEscapedQuotes = replace "\\\"" ""

-- TODO: Remove once merged https://github.com/crufter/haquery/pull/6
innerText :: HQ.Tag -> Text
innerText (HQ.Doctype _ text) = text
innerText (HQ.Text _ text) = text
innerText (HQ.Tag _ _ _ children) = DT.concat $ map innerText children

makeDiffChannel :: UTCTime -> Channel -> IO DiffChannel
makeDiffChannel current channel = do
  e <- try $ W.head_ $ unpack $ "http://nixos.org/channels/" <> name channel
  let link = case e of
               -- We get 302 redirect with Location header, no need to go further
               -- Propagate the rest of the errors
               Left (StatusCodeException _ headers _) -> C8.unpack $ snd $ head $ filter ((== "Location") . fst) headers
               Right response -> C8.unpack $ response ^. W.responseHeader "Location"
  let diff = diffUTCTime current <$> time channel
  return $ DiffChannel (name channel)
                       (diffToLabel diff)
                       (either id humanTimeDiff diff)
                       (either (const Nothing) Just diff)
                       (parseCommit link)
                       link
                       (jobset $ name channel)


parseCommit :: String -> String
parseCommit url = last $ splitOn "." url

-- |The list of the current NixOS channels
channels :: IO [DiffChannel]
channels = do
  r <- W.get "http://nixos.org/channels/"
  current <- getCurrentTime
  let html = pack $ show $ r ^. W.responseBody
  responseOrExc <- parallelE $ fmap (makeDiffChannel current) $ findGoodChannels $ html
  unless (null $ lefts responseOrExc) $ do
    print $ lefts responseOrExc
  return $ rights responseOrExc


humanTimeDiff :: NominalDiffTime -> Text
humanTimeDiff d
  | days > 1 = doShow days "days"
  | hours > 1 = doShow hours "hours"
  | minutes > 1 = doShow minutes "minutes"
  | otherwise = doShow d "seconds"
  where minutes = d / 60
        hours = minutes / 60
        days = hours / 24
        doShow x unit = (pack $ show $ truncate x) <> " " <> unit


jobset :: Text -> Maybe Text
jobset c
 | c == "nixos-unstable"       = Just "nixos/trunk-combined/tested"
 | c == "nixos-unstable-small" = Just "nixos/unstable-small/tested"
 | c == "nixpkgs-unstable"     = Just "nixpkgs/trunk/unstable"
 | "nixos-" `DT.isPrefixOf` c  = Just $ "nixos/release-" <> (DT.drop 6 c) <> "/tested"
 | otherwise                   = Nothing

-- | Takes time since last update to the channel and colors it based on it's age
diffToLabel :: Either Text NominalDiffTime -> Color
diffToLabel (Left _) = ""
diffToLabel (Right time)
  | days < 3 = "success"
  | days < 10 = "warning"
  | otherwise = "danger"
    where days = time / (60 * 60 * 24)
