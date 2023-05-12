#!/bin/bash
set -e

#disable
# DOCKER_REPO_URL=${DOCKER_REPO_URL:-085736401259.dkr.ecr.eu-west-1.amazonaws.com/}
# DOCKER_REPO_NAMESPACE=${DOCKER_REPO_NAMESPACE:-hyraxbio/}
# SHA=${SHA:-$(git rev-parse HEAD)}
#
# # Cleanup old dependency project
# \rm -rf buildDeps/depsProj
#
# # create new dependency project
# mkdir -p buildDeps/depsProj
# pushd buildDeps
# ./buildDepsProject-exe .. awssy ./depsProj
# popd
#
# docker build -t ${DOCKER_REPO_URL}${DOCKER_REPO_NAMESPACE}awssy:$SHA .
# docker tag ${DOCKER_REPO_URL}${DOCKER_REPO_NAMESPACE}awssy:$SHA awssy:latest
# docker tag ${DOCKER_REPO_URL}${DOCKER_REPO_NAMESPACE}awssy:$SHA ${DOCKER_REPO_URL}${DOCKER_REPO_NAMESPACE}awssy:latest
