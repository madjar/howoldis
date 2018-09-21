{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Web.Scotty (scotty, get, html, json)
import Text.PrettyPrint.Boxes (hsep, left, printBox, text, vcat)

import Control.Monad.Trans (liftIO)
import Data.List (transpose)
import Data.Text (Text, unpack)
import System.Environment (getArgs, getEnvironment)
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Channels (Channel (..), channels, jobset)

data RunType = Web | Console

main = do
  args <- getArgs
  case parseArgs args of
    Just Web     -> startWeb
    Just Console -> startConsole
    Nothing      -> print "unsupported flag"

startWeb :: IO ()
startWeb = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  scotty port $ do
    get "/" $ do
      allChannels <- liftIO channels
      html $ renderHtml $(shamletFile "index.hamlet")
    get "/api/channels" $ do
      allChannels <- liftIO channels
      json allChannels

startConsole :: IO ()
startConsole = channels >>= printTable . map attrs
 where
  attrs channel =
    [ unpack $ name channel
    , unpack $ humantime channel
    , commit channel
    ]
  printTable rows = printBox $ hsep 2 left $ map (vcat left . map text) (transpose rows)

parseArgs :: [String] -> Maybe RunType
parseArgs ["--server"] = Just Web
parseArgs []           = Just Console
parseArgs _            = Nothing
