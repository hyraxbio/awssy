#!/bin/sh
cp -r /root/_ssh /root/.ssh
chmod 600 /root/.ssh
/usr/bin/awssy --region Ireland --key /root/.ssh/hyraxbio.pem
