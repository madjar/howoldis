{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Channels (channels)
import Control.Monad.Trans (liftIO)
import System.Environment (getEnvironment)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamletFile)
import Web.Scotty

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  scotty port $ do
    get "/" $ do
      html $ renderHtml $(shamletFile "src/index.hamlet")
    get "/api/channels" $ do
      allChannels <- liftIO channels
      json allChannels
