#!/bin/bash -ex

ghc --make testapp1.hs
strip testapp1
tar czf ../testapp1.yesod *
