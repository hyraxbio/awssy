FROM hyrax_haskell_13_12 as build

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
