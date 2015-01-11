{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Web.Scotty

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Data.List (find)
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import System.Environment (getEnvironment)
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Channels (DiffChannel (..), channels, age, jobset)

main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  scotty port $ do
    get "/" $ do
      allChannels <- liftIO $ channels
      html $ renderHtml [shamlet|
$doctype 5
<head>
  <title> "How outdated are NixOS channels?"
  <link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css">
<body>
  <div class=container>
    <div class=page-header>
      <h1>How outdated are NixOS channels?
      <table class="table table-striped">
        <thead>
          <tr>
            <th>Channel
            <th>Updated
            <th>Tests
        <tbody>
          $forall chan <- allChannels
            <tr>
              <td>
                <a href="/#{dname chan}"> #{dname chan} 
              <td>
                #{dtime chan} ago
              <td>
                <a href="http://hydra.nixos.org/job/#{jobset chan}/tested#tabs-constituents"> Hydra
  <div class=container>
    <p class=text-muted>Made by <a href="https://twitter.com/georgesdubus">Georges Dubus</a>, code is on <a href="https://github.com/madjar/howoldis">github</a>
|]