{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Web.Scotty

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Data.List (find)
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import System.Environment (getEnvironment)
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.Posix.Signals (sigTERM, installHandler, Handler (CatchOnce))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import Channels (DiffChannel (..), channels, age, jobset)


main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  runInteruptable port $ do
    get "/:channel" $ do
      allChannels <- liftIO $ channels
      channelName <- param "channel"
      let mainChannel = findChannel channelName allChannels
      html $ renderHtml $(shamletFile "index.hamlet")


findChannel :: String -> [DiffChannel] -> DiffChannel
findChannel channelName chans = fromJust $
                                lookup channelName
                                <|> lookup "nixos-unstable"
                                <|> Just (head chans)
  where lookup n = find (\c -> dname c == n) chans


runInteruptable port app = do
  flag <- newEmptyMVar
  tid <- forkIO $ scotty port app
  installHandler sigTERM (CatchOnce $ do
      putStrLn "TERMinating"
      killThread tid
      putMVar flag ()) Nothing
  takeMVar flag
  putStrLn "TERMinated"
