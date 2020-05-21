FROM hyrax-haskell as build

WORKDIR /root/temp/code
COPY . /root/temp/
RUN stack build && stack install

FROM hyrax-base

RUN apt-get update && apt-get install -y netbase ca-certificates openssh-client locales locales-all mosh

#Set the locale
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

COPY ./runAwssy.sh /usr/bin/runAwssy
COPY ./settings.js /root/.local/share/awssy/settings.js

COPY --from=build /root/.local/bin/awssy usr/bin/awssy

WORKDIR /
ENTRYPOINT ["/usr/bin/runAwssy"]
