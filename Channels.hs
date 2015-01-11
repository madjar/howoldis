module Channels (Channel (..), channels, age, jobset) where

import Control.Monad (liftM)
import Data.List (isPrefixOf)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Data.Text (pack, unpack, strip)
import Text.HTML.TagSoup (sections, (~==), (~/=), innerText, parseTags)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (readTime)


data Channel = Channel { name :: String
                       , time :: UTCTime
                       } deriving (Show)


openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)

channelsPage :: IO String
channelsPage = openURL "http://nixos.org/channels/"

parseTime :: String -> UTCTime
parseTime = readTime defaultTimeLocale format . strip'
  where format = "%d-%b-%Y %R"
        strip' = unpack . strip . pack

-- |The list of the current NixOS channels
channels :: IO [Channel]
channels = liftM findGoodChannels channelsPage
  where findGoodChannels = filter isRealdDir . findChannels . parseTags
        isRealdDir channel = not $ "Parent" `isPrefixOf` name channel
        findChannels = map makeChannel . filter isNotHeader . sections (~== "<tr>")
        isNotHeader = (~/= "<th>") . head . drop 1
        makeChannel x = Channel name time
          where name = init . takeTextOf "<a>" $ x
                time = parseTime . takeTextOf "<td align=right>" $ x
                takeTextOf t = innerText . take 2 . dropWhile (~/= t)

age :: Channel -> IO String
age channel = do current <- getCurrentTime
                 let diff = diffUTCTime current (time channel)
                 return $ humanTimeDiff diff

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


jobset :: Channel -> String
jobset channel = j (name channel)
  where j "nixos-unstable" = "nixos/trunk-combined"
        j "nixos-unstable-small" = "nixos/unstable-small"
        j c | "nixos-" `isPrefixOf` c = "nixos/release-" ++ (drop 6 c)
        j _ = ""
