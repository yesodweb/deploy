#!/bin/bash -ex

mkdir -p /yesod-deploy/{bin,etc,incoming,unpacked}
cp angel reconfig unpacker /yesod-deploy/bin

apt-get install nginx

cat > /yesod-deploy/etc/angel.conf <<EOF
unpacker {
    exec = "/yesod-deploy/bin/unpacker /yesod-deploy/"
}
EOF

cat > /etc/init/yesod-deploy-angel.conf <<EOF
description "Angel process monitoring all deployed applications"
start on runlevel [2345];
stop on runlevel [!2345];
respawn
exec /yesod-deploy/bin/angel /yesod-deploy/etc/angel.conf
EOF

start yesod-deploy-angel
/etc/init.d/nginx start

touch /yesod-deploy/incoming/.trigger
