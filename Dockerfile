FROM alpine:3.9

COPY scripts/docker-install.sh /tmp/docker-install.sh
RUN /tmp/docker-install.sh
