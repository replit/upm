FROM ubuntu:19.04

COPY scripts/docker-install-build.bash /tmp/docker-install-build.bash
RUN /tmp/docker-install-build.bash

ENV PATH="/usr/local/go/bin:$PATH"

COPY . /upm
RUN make -C /upm upm

FROM ubuntu:19.04
COPY --from=0 /upm/cmd/upm/upm /usr/local/bin/upm
