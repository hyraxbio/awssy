#########################################################################
# Build 
#########################################################################
FROM 085736401259.dkr.ecr.eu-west-1.amazonaws.com/hyraxbio/hyrax-haskell as builder


RUN apt-get update && apt-get install -y libxss-dev

#------------
# cache some haskell dependencies
#------------
# Build a "cache layer"
# buildDepsProject-exe extacts all the dependencies from the cabal file and create a new dummy project
# This project can then be compiled in its own layer, so if there no cabal file changes then 
# compiling the project will use all the cached dependencies.

COPY ./buildDeps/depsProj/ /tmp/cacheProject/
WORKDIR /tmp/cacheProject
RUN stack build



#------------
# Now build awssy
# This looks strange as we are building things twice. But the layer above is just dependencies
# and wont need to be rebuilt often. Which means that the builds here are fast as the stack
# dependencies are already downloaded and build
#------------

WORKDIR /root/temp/code
COPY . /root/temp/
RUN stack build && stack install



#########################################################################
# Copy awssy & setup run container
#########################################################################
FROM 085736401259.dkr.ecr.eu-west-1.amazonaws.com/hyraxbio/hyrax-base

RUN apt-get update && apt-get install -y netbase ca-certificates openssh-client locales locales-all software-properties-common curl python3.8 python3.8-venv libx11-dev  libxrandr-dev libxinerama-dev libxss-dev && \
    apt-get autoremove --purge && \
    apt-get -y clean && \
    rm -rf /var/lib/apt/lists/*

RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 1


RUN curl "https://s3.amazonaws.com/aws-cli/awscli-bundle.zip" -o "awscli-bundle.zip"
RUN unzip awscli-bundle.zip 
RUN ./awscli-bundle/install -i /usr/local/aws -b /usr/local/bin/aws

#Set the locale
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8


COPY ./runAwssy.sh /usr/bin/runAwssy
COPY ./settings.js /root/.local/share/awssy/settings.js

COPY --from=builder /root/.local/bin/awssy /usr/bin/awssy

WORKDIR /
ENTRYPOINT ["/usr/bin/runAwssy"]
