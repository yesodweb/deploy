#!/bin/bash -ex

cabal clean
cabal configure
cabal build

rm -rf yesod-deploy
mkdir yesod-deploy

cp dist/build/deploy/deploy `which angel` yesod-deploy
strip yesod-deploy/*
cp setup.sh yesod-deploy

tar czfv yesod-deploy.tar.gz yesod-deploy
rm -rf yesod-deploy
