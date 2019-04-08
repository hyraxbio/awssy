FROM ubuntu:18.10 as build
RUN apt-get update && apt-get install -y \
    apt-utils \
    git \
    cmake \
    gcc \
    g++ \
    netbase \
    ca-certificates \
    wget \
    curl \
    vim 

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /root
RUN mkdir temp && \
    cd temp && \
    stack new init https://hyraxbio.github.io/hyraxHaskellSnapshots/hshyrax.hsfiles --resolver lts-13.12 && \
    cd init && \
    stack build

RUN cd temp/init && \
    stack install amazonka-ec2

RUN apt-get install -y libtinfo-dev

RUN cd temp/init && \
    stack install amazonka-s3 lens vty megaparsec text 

WORKDIR /root/temp/code
COPY ./temp/code /root/temp/
RUN cd /root/temp/code && \
    stack build && \
    stack install

FROM ubuntu:18.10

RUN apt-get update
RUN apt-get install -y netbase ca-certificates openssh-client

COPY ./runAwssy.sh /usr/bin/runAwssy
COPY ./settings.js /root/.local/share/awssy/settings.js

#COPY ./awssy /usr/bin/awssy
COPY --from=build /root/.local/bin/awssy usr/bin/awssy

WORKDIR /
ENTRYPOINT ["/usr/bin/runAwssy"]
