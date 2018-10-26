{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import           Channels (Channel(..), jobset, newEnv)
import qualified Channels (channels)
import           Control.Concurrent (ThreadId, threadDelay)
import           Control.Concurrent.STM
  ( atomically
  , newTVar
  , readTVarIO
  , writeTVar
  )
import           Control.Concurrent.Supervisor
  ( QueueLike
  , RestartStrategy(..)
  , Supervisor0
  , forkSupervised
  , newSupervisor
  , newSupervisorSpec
  )
import           Control.Monad (forever, void)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Control.Monad.Logger (logErrorN, logInfoN, runStderrLoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans (liftIO)
import           Control.Retry (RetryPolicyM, constantDelay)
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           System.Environment (getEnvironment)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet (shamletFile)
import           Web.Scotty

forkSupervised' ::
     (QueueLike q, MonadUnliftIO m)
  => Supervisor0 q
  -> RetryPolicyM IO
  -> m ()
  -> m ThreadId
forkSupervised' s p a = withRunInIO $ \run -> forkSupervised s p (run a)

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  supervisor <- newSupervisor =<< newSupervisorSpec OneForOne
  howoldisEnv <- newEnv
  flip runReaderT howoldisEnv $
    runStderrLoggingT $ do
      initial <- Channels.channels
      channels <-
        liftIO . atomically . newTVar $
        case initial of
          Right xs -> xs
          Left _ -> []
      void $
        forkSupervised' supervisor (constantDelay 10000000) $
        forever $ do
          liftIO . threadDelay $ 60 * 1000000
          new <- Channels.channels
          case new of
            Left e -> do
              logErrorN $
                "Failed to update channels: " <> (Text.pack . show $ e)
              error "restart me please"
            Right xs -> liftIO . atomically . writeTVar channels $ xs
          logInfoN "Updated channels"
      liftIO . scotty port $ do
        get "/" $ do
          allChannels <- liftIO . readTVarIO $ channels
          html $ renderHtml $(shamletFile "index.hamlet")
        get "/api/channels" $ do
          allChannels <- liftIO . readTVarIO $ channels
          json allChannels
