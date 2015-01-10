{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Web.Scotty

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Data.List (find)
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Channels (Channel (..), channels, age, jobset)

main = scotty 3000 $ do
  get "/:channel" $ do
    channelName <- param "channel"
    allChannels <- liftIO $ channels
    let channel = findChannel channelName allChannels
    channelAge <- liftIO $ age channel
--     html $ renderHtml $ mainPage (name channel) channelAge (map name allChannels)
    html $ renderHtml [shamlet|
$doctype 5
<head>
  <title> "How old are NixOS channels?"
  <link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css">
<body>
  <div class=container>
    <div class=page-header>
      <h1>How old is #{name channel}?
    <h2 class=text-center>#{channelAge} old
    $if not $ null $ jobset channel
      <p>If that's too old, you might want to take a look at:
        <ul>
          <li>
            <a href="http://hydra.nixos.org/jobset/#{jobset channel}"> the last evaluations
          <li>
            <a href="http://hydra.nixos.org/jobset/#{jobset channel}/tested#tabs-constituents"> the test suite
    <p>You can also check the other channels:
      <ul>
        $forall chan <- allChannels
          <li>
            <a href="/#{name chan}"> #{name chan}
  <div class=container>
    <p class=text-muted>Made by <a href="https://twitter.com/georgesdubus">Georges Dubus</a>, code is on <a href="https://github.com/madjar/howoldis">github</a>
|]

findChannel :: String -> [Channel] -> Channel
findChannel channelName chans = fromJust $
                                lookup channelName
                                <|> lookup "nixos-unstable"
                                <|> Just (head chans)
  where lookup n = find (\c -> name c == n) chans
