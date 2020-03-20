# How old is that NixOS Channel?

**IMPORTANT NOTE**: Following an [update to the nixos.org](https://discourse.nixos.org/t/announcement-moving-nixos-org-to-netlify/6212/27) website, this project doesn't work anymore. However, https://status.nixos.org/ may be exactly what you're looking for!

A small web application that gives the sort answer to that question, and some links to the long answers.

My first web application in Haskell, built with scotty.

It is running at http://howoldis.herokuapp.com

# Development


    nix-shell -p stack --run "stack build --nix --file-watch --exec howoldis"
