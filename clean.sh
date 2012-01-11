#!/bin/bash -ex

rm -rf /yesod-deploy
stop yesod-deploy-angel || true
rm -f /etc/init/yesod-deploy-angel.conf
rm -f /etc/nginx/sites-enabled/yesod-deploy.conf
/etc/init.d/nginx reload
