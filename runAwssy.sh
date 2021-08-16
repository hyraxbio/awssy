#!/bin/sh
cp -r /root/_ssh /root/.ssh
chmod 600 /root/.ssh
eval `ssh-agent`
/usr/bin/awssy --key /root/.ssh/hyraxbio.pem "$@"
