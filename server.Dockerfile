FROM ubuntu:18.04

ARG BIN_DIR

RUN mkdir -p /opt/ferry-services

WORKDIR /opt/ferry-services

RUN apt-get update && \
  apt-get install -y libpq5 netbase ca-certificates && \
  rm -rf /var/lib/apt/lists/*

COPY $BIN_DIR .

CMD ["/opt/ferry-services/ferry-services-server-exe"]