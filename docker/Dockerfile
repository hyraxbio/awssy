FROM ubuntu:19.04 as build

ENV TZ=Africa/Johannesburg
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
ENV DEBIAN_FRONTEND=noninteractive 

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
    vim \
    libtinfo-dev

RUN apt-get update && apt-get install -y postgresql-server-dev-all libpq-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /root
RUN mkdir temp && \
    cd temp && \
    stack new init https://hyraxbio.github.io/hyraxHaskellSnapshots/hshyrax.hsfiles --resolver lts-15.7 && \
    cd init && \
    stack build

#RUN cd /root/temp/ && \
#    git clone https://github.com/hyraxbio/amazonka.git && \
#    cd amazonka && \
#    git reset aebaac9ee297821ffc831d411e9f6c10a2604b70 --hard
#
#RUN cd /root/temp/amazonka/amazonka-ec2 && \
#    stack build --resolver lts-15.7

WORKDIR /root/temp/code
COPY ./temp/code /root/temp/
RUN cd /root/temp/code && \
    stack build && \
    stack install

FROM ubuntu:19.04

RUN apt-get update
RUN apt-get install -y netbase ca-certificates openssh-client

COPY ./runAwssy.sh /usr/bin/runAwssy
COPY ./settings.js /root/.local/share/awssy/settings.js

COPY --from=build /root/.local/bin/awssy usr/bin/awssy

WORKDIR /
ENTRYPOINT ["/usr/bin/runAwssy"]
