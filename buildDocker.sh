#!/bin/sh
set -e

\rm -rf temp
mkdir -p temp/code

rsync -av ../ temp/code/ --exclude='docker' --exclude='.stack-work' --exclude='.git' 
docker build --tag=awssy$1 .
docker tag awssy$1 localhost:5987/awssy$1
docker push localhost:5987/awssy$1
