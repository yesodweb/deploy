#!/bin/bash -xe

sudo rm -rf /yesod-deploy
sudo stop yesod-deploy-angel || true
sudo rm -f /etc/init/yesod-deploy-angel.conf
sudo rm -f /etc/nginx/sites-enabled/yesod-deploy.conf
sudo /etc/init.d/nginx reload

ghc -Wall -Werror --make deploy.hs && strip deploy
sudo ./setup.sh
