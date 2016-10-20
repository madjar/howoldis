# How old is that NixOS Channel?

A small web application that gives the sort answer to that question, and some links to the long answers.

My first web application in Haskell, built with scotty.

It is running at http://howoldis.herokuapp.com

# Development


    nix-shell -p stack --run "stack build --nix --file-watch --exec howoldis"
