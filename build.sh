#!/bin/bash -xe

sudo ./clean.sh

cabal build
cp dist/build/deploy/deploy .
strip deploy

sudo ./setup.sh
