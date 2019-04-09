#!/bin/sh
set -e

rm -rf temp
mkdir -p temp/code

git clone git@github.com:hyraxbio/hyraxDocker.git temp/hyraxDocker
cd temp/hyraxDocker
./Dockerfile.haskell_13_12.sh

cd ../..
rsync -av ../ temp/code/ --exclude='docker' --exclude='.stack-work' --exclude='.git' 
docker build --tag=awssy$1 .
docker tag awssy$1 localhost:5987/awssy$1
docker push localhost:5987/awssy$1
