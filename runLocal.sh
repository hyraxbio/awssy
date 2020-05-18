#!/bin/sh
docker run -v ~/.ssh:/root/_ssh -v ~/.aws:/root/.aws -ti --network=host awssy
