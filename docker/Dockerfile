FROM ubuntu:18.10

RUN apt-get update
RUN apt-get install -y netbase ca-certificates openssh-client

COPY ./awssy /usr/bin/awssy
COPY ./runAwssy.sh /usr/bin/runAwssy
COPY ./settings.js /root/.local/share/awssy/settings.js

WORKDIR /
ENTRYPOINT ["/usr/bin/runAwssy", "--region", "Ireland", "--key", "/root/.ssh/hyraxbio.pem"]
