module Channels (DiffChannel (..), channels, jobset) where

import Control.Lens
import qualified Haquery as HQ
import Network.Wreq
import Data.List (isPrefixOf)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.LocalTime (localTimeToUTC, hoursToTimeZone)
import Data.Time.Zones
import Data.Time.Zones.All
import qualified Data.Text
import Data.Text (pack, unpack, Text)


data Channel = Channel { name :: String
                       , time :: Either String UTCTime
                       } deriving (Show)

data DiffChannel = DiffChannel { dname  :: String
                               , dlabel :: Color
                               , dtime  :: String
                               } deriving (Show)

type Color = String


parseTime :: String -> Either String UTCTime
parseTime = fmap (localTimeToUTCTZ tz) . parseTimeM True defaultTimeLocale "%F %R"
  where tz = tzByLabel Europe__Rome -- CET/CEST

findGoodChannels :: String -> [Channel]
findGoodChannels html = map makeChannel rows
  where
    rows = drop 2 $ concatMap (HQ.select (pack "tr:has(a)")) $ HQ.parseHtml $ pack html
    makeChannel tag = Channel name time
      where name = maybe "" unpack $ HQ.attr (pack "href") $ head $ HQ.select (pack "a") tag
            time = parseTime $ unpack $ innerText $ head $ HQ.select (pack "*:nth-child(3)") tag

-- TODO: Remove once merged https://github.com/crufter/haquery/pull/6
innerText :: HQ.Tag -> Text
innerText (HQ.Doctype _ text) = text
innerText (HQ.Text _ text) = text
innerText (HQ.Tag _ _ _ children) = Data.Text.concat $ map innerText children

makeDiffChannel :: Channel -> IO DiffChannel
makeDiffChannel channel = do
  current <- getCurrentTime
  let diff = diffUTCTime current <$> time channel
  return $ DiffChannel (name channel) (diffToLabel diff) (either id humanTimeDiff diff)

-- |The list of the current NixOS channels
channels :: IO [DiffChannel]
channels = do
  r <- get "http://nixos.org/channels/"
  mapM makeDiffChannel $ findGoodChannels $ show $ r ^. responseBody


humanTimeDiff :: NominalDiffTime -> String
humanTimeDiff d
  | days > 1 = doShow days "days"
  | hours > 1 = doShow hours "hours"
  | minutes > 1 = doShow minutes "minutes"
  | otherwise = doShow d "seconds"
  where minutes = d / 60
        hours = minutes / 60
        days = hours / 24
        doShow x unit = (show $ truncate x) ++ " " ++ unit


jobset :: DiffChannel -> Maybe String
jobset channel
 | c == "nixos-unstable"       = Just "nixos/trunk-combined/tested"
 | c == "nixos-unstable-small" = Just "nixos/unstable-small/tested"
 | c == "nixpkgs-unstable"     = Just "nixpkgs/trunk/unstable"
 | "nixos-" `isPrefixOf` c     = Just $ "nixos/release-" ++ (drop 6 c) ++ "/tested"
 | otherwise                   = Nothing
 where
   c = dname channel

-- | Takes time since last update to the channel and colors it based on it's age
diffToLabel :: Either String NominalDiffTime -> Color
diffToLabel (Left _) = ""
diffToLabel (Right time)
  | days < 3 = "success"
  | days < 10 = "warning"
  | otherwise = "danger"
    where days = time / (60 * 60 * 24)
